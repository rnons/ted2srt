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

const isProd = (process.env.NODE_ENV === 'production') ? true : false;

gulp.task('scripts', function () {
  var dest = isProd ? 'dist' : '.tmp';
  return browserify('app/scripts/main.js', { debug: true })
    .transform(babelify)
    .bundle()
    .on('error', function (err) { console.log('Error : ' + err.message); })
    .pipe(source('scripts/main.js'))
    .pipe(buffer())
    .pipe($.if(isProd, $.uglify()))
    .pipe(gulp.dest(dest))
    .pipe(reload({stream: true}));
});

gulp.task('styles', function () {
  var dest = isProd ? 'dist' : '.tmp';
  return gulp.src('app/styles/main.less', {base: 'app'})
    .pipe($.less({
      paths: ['.']
    }))
    .pipe($.postcss([
      require('autoprefixer-core')({browsers: ['last 1 version']})
    ]))
    .pipe($.sourcemaps.write())
    .pipe(gulp.dest(dest))
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

gulp.task('html', ['scripts', 'styles'], function () {
  var assets = $.useref.assets({searchPath: ['.tmp', '.']});

  return gulp.src('app/*.html')
    .pipe(assets)
    .pipe($.if('**/vendor.js', $.uglify()))
    .pipe($.if('*.css', $.csso()))
    .pipe(assets.restore())
    .pipe($.useref())
    .pipe($.if('*.html', $.minifyHtml({conditionals: true, loose: true})))
    .pipe(gulp.dest('dist'));
});

gulp.task('clean', require('del').bind(null, ['.tmp', 'dist']));

gulp.task('serve', ['scripts', 'styles'], function () {
  var proxyOptions = url.parse('http://localhost:3001');
  proxyOptions.route = '/api';
  browserSync({
    ghostMode: false,
    notify: false,
    open: false,
    port: 9000,
    server: {
      baseDir: ['.tmp', 'app'],
      middleware: [proxy(proxyOptions)]
    }
  });

  // watch for changes
  gulp.watch('app/*.html').on('change', reload);

  gulp.watch('app/scripts/**/*.js', ['scripts']);
  gulp.watch('app/styles/**/*.less', ['styles']);
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
