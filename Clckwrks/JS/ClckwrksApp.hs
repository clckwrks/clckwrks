{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Clckwrks.JS.ClckwrksApp where

import Language.Javascript.JMacro

clckwrksAppJS :: Bool -> JStat
clckwrksAppJS True = [jmacro|
  {
    var clckwrksApp = angular.module('clckwrksApp', [
      'happstackAuthentication',
      'usernamePassword',
      'openId',
      'ngRoute'
    ]);

    clckwrksApp.config(['$routeProvider',
      function($routeProvider) {
        $routeProvider.when('/resetPassword',
                             { templateUrl: '/authenticate/authentication-methods/password/partial/reset-password-form',
                               controller: 'UsernamePasswordCtrl'
                             });
      }]);

    clckwrksApp.controller('ClckwrksCtrl', ['$scope', '$http',function($scope, $http) {
     return;
    }]);
  }
 |]
clckwrksAppJS False = [jmacro|
  {
    var clckwrksApp = angular.module('clckwrksApp', [
      'happstackAuthentication',
      'usernamePassword',
      'ngRoute'
    ]);

    clckwrksApp.config(['$routeProvider',
      function($routeProvider) {
        $routeProvider.when('/resetPassword',
                             { templateUrl: '/authenticate/authentication-methods/password/partial/reset-password-form',
                               controller: 'UsernamePasswordCtrl'
                             });
      }]);

    clckwrksApp.controller('ClckwrksCtrl', ['$scope', '$http',function($scope, $http) {
     return;
    }]);
  }
 |]
