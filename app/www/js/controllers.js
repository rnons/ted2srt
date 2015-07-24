angular.module('reted.controllers', [])

.controller('TalksCtrl', function($scope, $http, Talks) {
  $scope.model = {}
  $scope.model.hasMore = true;

  $scope.loadMore = function() {
    Talks.loadMore().then(function(data) {
      $scope.$broadcast('scroll.infiniteScrollComplete');
      $scope.talks = Talks.all();
      if (data.length < 10) {
        $scope.model.hasMore = false;
        $scope.$applyAsync();
      }
    });
  }
})

.controller('TalkCtrl', function($scope, $stateParams, API) {
  var slug = $stateParams.slug;
  var talk = null;
  API.getTalk({slug: slug}).$promise
    .then(function (data) {
      $scope.talk = data;
      $scope.talk.speaker = $scope.talk.name.split(':')[0];
      $scope.talk.title = $scope.talk.name.split(':')[1];
      return API.getTalkTranscript({id: $scope.talk.id}).$promise
    })
    .then(function (data) {
      $scope.talk.transcript = '<p>' + data.text.split('\n\n').join('</p><p>') + '</p>';
    });
})

.controller('SearchInputCtrl', function($scope, $state) {
  $scope.model = {};
  $scope.search = function() {
    if ($scope.model.q.trim().length) {
      $state.go('search', {q: $scope.model.q})
    }
  }
})

.controller('SearchCtrl', function($scope, $stateParams, API) {
  $scope.model = {};
  $scope.model.hasMore = false;

  API.search({q: $stateParams.q}).$promise.then(function(data) {
    $scope.talks = data;
  });
});
