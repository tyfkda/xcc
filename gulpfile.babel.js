'use strict'

import gulp from 'gulp'
const browserSync = require('browser-sync').create()

// TypeScript
import clone from 'clone'
import eslint from 'gulp-eslint'
import webpack from 'webpack'
import webpackStream from 'webpack-stream'
import webpackConfig from './webpack.config.babel'

// HTML
import ejs from 'gulp-ejs'
import htmlmin from 'gulp-htmlmin'

// SASS
import sassPlugin from 'sass'
import gulpSass from 'gulp-sass'
import cssnano from 'gulp-cssnano'

// // Unit test
// const jest = require('gulp-jest').default

import fs from 'fs'

const ROOT_DIR = `${__dirname}`
const DEST_DIR = `${ROOT_DIR}/public`
const ASSETS_DIR = `${DEST_DIR}`
const SRC_TS_DIR = `${ROOT_DIR}/src/wcc/www`
const SRC_TS_FILES = `${SRC_TS_DIR}/**/*.ts`
const SRC_HTML_DIR = `${ROOT_DIR}/src/wcc/www`
const SRC_HTML_FILES = `${SRC_HTML_DIR}/*.html`
const SRC_SASS_FILES = `${ROOT_DIR}/src/wcc/www/**/*.scss`
// const SRC_TEST_DIR = `${ROOT_DIR}/test`
// const SRC_TEST_FILES = `${SRC_TEST_DIR}/**/*.spec.ts`
const RELEASE_DIR = `${ROOT_DIR}/release`
const RELEASE_ASSETS_DIR = `${RELEASE_DIR}`

function convertHtml(buildTarget, dest) {
  return gulp.src([SRC_HTML_FILES,
                   `!${SRC_HTML_DIR}/**/_*.html`])
    .pipe(ejs({buildTarget}))
    .pipe(htmlmin({
      collapseWhitespace: true,
      removeComments: true,
      minifyCSS: true,
      minifyJS: true,
      removeAttributeQuotes: true,
    }))
    .pipe(gulp.dest(dest))
}

function checkLint(glob) {
  return gulp.src(glob)
    .pipe(eslint())
    .pipe(eslint.format())
    .pipe(eslint.failAfterError())
}

function buildWhenModified(glob, buildFunc) {
  return gulp.watch(glob, buildFunc)
}

export function reload(done) {
  browserSync.reload()
  done()
}

export function watchReload() {
  return gulp.watch([`${DEST_DIR}/*.html`,
                     `${DEST_DIR}/**/*.js`],
                    gulp.series([reload]))
}

export function html() {
  return convertHtml('debug', DEST_DIR)
}

export function watchHtml() {
  return gulp.watch(SRC_HTML_FILES, gulp.series(html))
}

export function ts() {
  const config = clone(webpackConfig)
  config.mode = 'development'
  config.devtool = 'source-map'
  return gulp.src([`${SRC_TS_DIR}/main.ts`])
    .pipe(webpackStream(config, webpack))
    .pipe(gulp.dest(ASSETS_DIR))
}

export function watchTs() {
  const config = clone(webpackConfig)
  config.mode = 'development'
  config.watch = true
  config.devtool = 'source-map'
  return gulp.src(SRC_TS_FILES, {base: SRC_TS_DIR})
    .pipe(webpackStream(config, webpack))
    .pipe(gulp.dest(ASSETS_DIR))
}

export function sass() {
  return gulp.src(SRC_SASS_FILES)
    .pipe(gulpSass(sassPlugin)())
    .pipe(cssnano())
    .pipe(gulp.dest(ASSETS_DIR))
    .pipe(browserSync.stream())
}

export function watchSass() {
  return gulp.watch(SRC_SASS_FILES, gulp.series(sass))
}

const LINT_GLOBS = [
  SRC_TS_FILES,
  // SRC_TEST_FILES,
]

export function lint() {
  return checkLint(LINT_GLOBS)
}

export function watchLint() {
  return gulp.watch(LINT_GLOBS, lint)
}

export function server() {
  browserSync.init({
    server: {
      baseDir: DEST_DIR,
      index: 'index.html',
    },
    open: false,
  })
}

// // Unit test.
// export function test() {
//   return gulp.src(SRC_TEST_DIR)
//     .pipe(jest({
//       "transform": {
//         "^.+\\.tsx?$": "ts-jest",
//       },
//       "testRegex": "(/__tests__/.*|(\\.|/)(test|spec))\\.(jsx?|tsx?)$",
//       "moduleFileExtensions": [
//         "ts",
//         "tsx",
//         "js",
//         "jsx",
//         "json",
//         "node",
//       ],
//     }))
// }
// export function watchTest() {
//   return gulp.watch([SRC_TS_FILES,
//                      SRC_TEST_FILES],
//                     gulp.series(test))
// }

export async function clean(done) {
  await Promise.all([
    fs.promises.rm(DEST_DIR, { recursive: true, force: true }),
    fs.promises.rm(RELEASE_DIR, { recursive: true, force: true }),
  ])
  return done()
}

export const watch = gulp.parallel(watchHtml, watchTs, watchSass,
                                   watchLint, // watchTest,
                                   watchReload)

export const build = gulp.parallel(html, ts, sass, lint)

exports.default = gulp.parallel(build, server, watch)

export const releaseBuild = gulp.series(sass, () => {
  // Copy resources.
  return gulp.src([`${DEST_DIR}/**/*.*`,
                   `!${DEST_DIR}/index.html`,
                   `!${DEST_DIR}/**/*.js`,
                   `!${DEST_DIR}/**/*.map`,
                  ],
                  {base: DEST_DIR})
    .pipe(gulp.dest(RELEASE_DIR))
})

export function releaseHtml() {
  // Build HTML for release.
  return convertHtml('release', RELEASE_DIR)
}

export function releaseTs() {
  // Concatenate TypeScript into single 'assets/main.ts' file.
  const config = clone(webpackConfig)
  // delete config.output.sourceMapFilename
  return gulp.src(`${SRC_TS_DIR}/main.ts`)
    .pipe(webpackStream(config, webpack))
    .pipe(gulp.dest(RELEASE_ASSETS_DIR))
}

export const release = gulp.parallel(releaseBuild, releaseHtml, releaseTs)
