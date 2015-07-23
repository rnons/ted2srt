angular.module('reted.controllers', [])

.controller('TalksCtrl', function($scope, $http, Talks) {
  $scope.model = {}
  $scope.model.hasMore = true;

  $scope.loadMore = function() {
    Talks.loadMore().then(function(response) {
      $scope.$broadcast('scroll.infiniteScrollComplete');
      $scope.talks = Talks.all();
      if (response.data.length < 10) {
        $scope.model.hasMore = false;
        $scope.$applyAsync();
      }
    });
  }
})

.controller('TalkCtrl', function($scope, $http, $stateParams) {
  var slug = $stateParams.slug;
  var talk = null;
  $http.get('/api/talks/' + slug)
    .then(function (response) {
      $scope.talk = response.data;
      $scope.talk.speaker = $scope.talk.name.split(':')[0];
      $scope.talk.title = $scope.talk.name.split(':')[1];
      return $http.get('/api/talks/' + $scope.talk.id + '/transcripts/txt?lang=en');
    })
    .then(function (response) {
      $scope.talk.transcript = '<p>' + response.data.split('\n\n').join('</p><p>') + '</p>';
    });
});
