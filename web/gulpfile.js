var gulp = require('gulp');
var $ = require('gulp-load-plugins')();
var babelify = require('babelify');
var browserify = require('browserify');
var browserSync = require('browser-sync');
var buffer = require('vinyl-buffer');
var proxy = require('proxy-middleware');
var reload = browserSync.reload;
var source = require('vinyl-source-stream');
var url = require('url');

const DEST = './dist';
const isProd = (process.env.NODE_ENV === 'production') ? true : false;

gulp.task('scripts', function () {
  return browserify('app/scripts/main.js', { debug: true })
    .transform(babelify)
    .bundle()
    .on('error', function (err) { console.log('Error : ' + err.message); })
    .pipe(source('scripts/main.js'))
    .pipe(buffer())
    .pipe($.if(isProd, $.uglify()))
    .pipe(gulp.dest(DEST))
    .pipe(reload({stream: true}));
});

gulp.task('styles', function () {
  return gulp.src('app/styles/main.css', {base: 'app'})
    .pipe($.cssnext({
      compress: true
    }))
    .pipe(gulp.dest(DEST))
    .pipe(reload({stream: true}));
});

gulp.task('jshint', function () {
  return gulp.src([
      'app/scripts/**/*.js',
      'test/**/*.js'
    ])
    .pipe(reload({stream: true, once: true}))
    .pipe($.jshint())
    .pipe($.jshint.reporter('jshint-stylish'))
    .pipe($.if(!browserSync.active, $.jshint.reporter('fail')));
});

gulp.task('assets', function () {
  return gulp.src([
    'app/favicon.ico',
    'app/search.xml'
    ])
    .pipe(gulp.dest(DEST));
});

gulp.task('html', function () {
  return gulp.src('app/*.html')
    .pipe($.minifyHtml({conditionals: true, loose: true}))
    .pipe(gulp.dest(DEST))
    .pipe(reload({stream: true}));
});

gulp.task('clean', require('del').bind(null, ['dist']));

gulp.task('serve', ['styles', 'scripts', 'assets', 'html'], function () {
  var proxyOptions = url.parse('http://localhost:3001');
  proxyOptions.route = '/api';
  browserSync({
    ghostMode: false,
    notify: false,
    open: false,
    port: 9000,
    server: {
      baseDir: [DEST],
      middleware: [proxy(proxyOptions)]
    }
  });

  // watch for changes
  gulp.watch('app/*.html', ['html']);
  gulp.watch('app/scripts/**/*.js', ['scripts']);
  gulp.watch('app/styles/**/*.css', ['styles']);
});

gulp.task('build', isProd ? ['jshint', 'html'] : null, function () {
  if (!isProd) {
    throw new Error('Requires NODE_ENV set to production, run `NODE_ENV=production gulp build`');
  }
  return gulp.src('dist/**/*').pipe($.size({title: 'build', gzip: true}));
});

gulp.task('default', ['clean'], function () {
  gulp.start('build');
});
