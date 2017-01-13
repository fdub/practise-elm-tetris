var gulp = require('gulp');
var elm  = require('gulp-elm');

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
  return gulp.src('src/Main.elm')
    .pipe(elm())
    .pipe(gulp.dest('output/'));
});

gulp.task('elm:watch', function () {
  var watcher = gulp.watch(['src/**/*.elm', 'index.html'], ['elm']);
});