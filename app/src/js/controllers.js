angular.module('reted.controllers', [])

.controller('MainController', function($scope, $state) {
  var self = this;
  self.subheaderShown = false;

  self.search = function() {
    if (self.q.trim().length) {
      $state.go('search', {q: self.q})
    }
  };

  $scope.$on('$stateChangeSuccess', function() {
    if ($state.current.name !== 'search') {
      self.subheaderShown = false;
    }
  });
})

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

.controller('TalkCtrl', function($scope, $stateParams, $interval, $filter, API) {
  $scope.model = {}
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

  var mediaStatusCallback = function(status) {
    var duration;
    if (status === 1) {
      $scope.model.audioPlayerShown = true;
      $scope.model.audioProgress = 'Loading...';
    } else if (status === 2) {
      $scope.model.audioPlayerShown = true;
      $scope.model.audioProgress = 'Loading...';
      $scope.model.playButtonShown = true;
      $scope.model.playButtonClass = 'ion-pause';
      $interval(function() {
        $scope.media.getCurrentPosition(function(pos) {
          if (!duration) {
            duration = $filter('seconds')($scope.media.getDuration());
          }
          $scope.model.audioProgress = $filter('seconds')(pos) + '/' + duration;
        });
      }, 1000);
    }
    $scope.$applyAsync();
  }

  $scope.playAudio = function() {
    if (!$scope.media) {
      var src = 'http://download.ted.com/talks/' + $scope.talk.mSlug + '.mp3';
      if (window.Media) {
        $scope.media = new Media(src, null, null, mediaStatusCallback);
      } else {
        $scope.media = new Audio(src);
      }
    }
    $scope.media.play();
  };

  $scope.stopAudio = function() {
    $scope.media.stop();
    $scope.model.audioPlayerShown = false;
  };

  $scope.toggleAudio = function() {
    if ($scope.model.playButtonClass === 'ion-pause') {
      $scope.media.pause();
      $scope.model.playButtonClass = 'ion-play';
    } else {
      $scope.media.play();
      $scope.model.playButtonClass = 'ion-pause';
    }
  };

  $scope.$on('$destroy', function() {
    $scope.media.release();
  });
})

.controller('SideMenuCtrl', function($scope, $state, $ionicSideMenuDelegate, API) {
  $scope.getRandomTalk = function() {
    API.getRandomTalk().$promise.then(function(data) {
      $state.go('talks', {slug: data.slug}).then(function() {
        $ionicSideMenuDelegate.toggleLeft();
      });
    });
  };
})

.controller('SearchCtrl', function($scope, $stateParams, API) {
  $scope.model = {};
  $scope.model.hasMore = false;

  API.search({q: $stateParams.q}).$promise.then(function(data) {
    $scope.talks = data;
  });
});
