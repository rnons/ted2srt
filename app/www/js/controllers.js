angular.module('starter.controllers', [])

.controller('TalksCtrl', function($scope, $http) {
  $http.get('/api/talks')
    .success(function (data) {
      console.log(data);
      $scope.talks = data;
    });
})

.controller('TalkCtrl', function($scope) {
  console.log('talk page');
})
