let gulp = require('gulp');
let elm = require('gulp-elm');
let bs =require('browser-sync')({
  server: {
    baseDir: "www"
  }
});

gulp.task('elm', () => {
  return gulp.src('src/Main.elm')
    .pipe(elm({filetype: 'html'}))
    .on('error', (err) => {
      console.error(err.message);
    })
    .pipe(gulp.dest('www'))
});

gulp.task('reload', () => {
  return new Promise((resolve, reject) => {
    bs.reload()
    resolve()
  });
});

gulp.task('default', () => {
  gulp.watch(['src/**'], gulp.series("elm", "reload"));
});
