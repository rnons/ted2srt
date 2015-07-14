var gulp = require('gulp');
var $ = require('gulp-load-plugins')();
var babelify = require('babelify');
var browserify = require('browserify');
var buffer = require('vinyl-buffer');
var source = require('vinyl-source-stream');
var browserSync = require('browser-sync');
var reload = browserSync.reload;

const isProd = (process.env.NODE_ENV === 'production') ? true : false;

gulp.task('scripts', function () {
  return browserify('app/scripts/main.js', { debug: true })
    .transform(babelify)
    .bundle()
    .on('error', function (err) { console.log('Error : ' + err.message); })
    .pipe(source('main.js'))
    .pipe(buffer())
    .pipe($.if(isProd, $.uglify()))
    .pipe(gulp.dest('.tmp/scripts'))
    .pipe(reload({stream: true}));
});

gulp.task('styles', function () {
  return gulp.src('app/styles/main.less')
    .pipe($.less({
      paths: ['.']
    }))
    .pipe($.postcss([
      require('autoprefixer-core')({browsers: ['last 1 version']})
    ]))
    .pipe($.sourcemaps.write())
    .pipe(gulp.dest('.tmp/styles'))
    .pipe(reload({stream: true}));
});

gulp.task('jshint', function () {
  return gulp.src('app/scripts/**/*.js')
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

gulp.task('images', function () {
  return gulp.src('app/images/**/*')
    .pipe($.cache($.imagemin({
      progressive: true,
      interlaced: true,
      // don't remove IDs from SVGs, they are often used
      // as hooks for embedding and styling
      svgoPlugins: [{cleanupIDs: false}]
    })))
    .pipe(gulp.dest('dist/images'));
});

gulp.task('fonts', function () {
  return gulp.src(require('main-bower-files')({
    filter: '**/*.{eot,svg,ttf,woff,woff2}'
  }).concat('app/fonts/**/*'))
    .pipe(gulp.dest('.tmp/fonts'))
    .pipe(gulp.dest('dist/fonts'));
});

gulp.task('extras', function () {
  return gulp.src([
    'app/*.*',
    '!app/*.html'
  ], {
    dot: true
  }).pipe(gulp.dest('dist'));
});

gulp.task('clean', require('del').bind(null, ['.tmp', 'dist']));

gulp.task('serve', ['scripts', 'styles', 'fonts'], function () {
  browserSync({
    ghostMode: false,
    notify: false,
    open: false,
    port: 9000,
    server: {
      baseDir: ['.tmp', 'app'],
      routes: {
        '/bower_components': 'bower_components'
      },
      middleware: [
        require('connect-modrewrite')([
          '!.*\.(js|css).*$ /index.html [L]'
        ])
      ]
    }
  });

  // watch for changes
  gulp.watch([
    'app/*.html',
    'app/images/**/*',
    '.tmp/fonts/**/*'
  ]).on('change', reload);

  gulp.watch('app/scripts/**/*.js', ['scripts']);
  gulp.watch('app/styles/**/*.less', ['styles']);
  gulp.watch('app/fonts/**/*', ['fonts']);
  gulp.watch('bower.json', ['wiredep', 'fonts']);
});

// inject bower components
gulp.task('wiredep', function () {
  var wiredep = require('wiredep').stream;

  gulp.src('app/styles/*.less')
    .pipe(wiredep({
      ignorePath: /^(\.\.\/)+/
    }))
    .pipe(gulp.dest('app/styles'));

  gulp.src('app/*.html')
    .pipe(wiredep({
      ignorePath: /^(\.\.\/)*\.\./
    }))
    .pipe(gulp.dest('app'));
});

gulp.task('build', isProd ? ['jshint', 'html', 'images', 'fonts', 'extras'] : null, function () {
  if (!isProd) {
    throw new Error('Requires NODE_ENV set to production, run `NODE_ENV=production gulp build`');
  }
  return gulp.src('dist/**/*').pipe($.size({title: 'build', gzip: true}));
});

gulp.task('default', ['clean'], function () {
  gulp.start('build');
});
