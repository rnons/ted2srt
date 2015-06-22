// Ionic Starter App

// angular.module is a global place for creating, registering and retrieving Angular modules
angular.module('reted', ['ionic', 'reted.controllers'])

.run(function($ionicPlatform) {
  $ionicPlatform.ready(function() {
    // Hide the accessory bar by default (remove this to show the accessory bar above the keyboard
    // for form inputs)
    if(window.cordova && window.cordova.plugins.Keyboard) {
      cordova.plugins.Keyboard.hideKeyboardAccessoryBar(true);
    }
    if(window.StatusBar) {
      StatusBar.styleDefault();
    }
  });
})

.config(function($stateProvider, $urlRouterProvider) {
  $stateProvider
    .state('index', {
      url: '',
      templateUrl: 'templates/talks.html',
      controller: 'TalksCtrl'
    })
    .state('talks', {
      url: '/talks/:slug',
      templateUrl: 'templates/talk.html',
      controller: 'TalkCtrl'
    });

  $urlRouterProvider.otherwise('/');
});
