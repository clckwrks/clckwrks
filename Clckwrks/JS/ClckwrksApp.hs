{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Clckwrks.JS.ClckwrksApp where

import Language.Javascript.JMacro

clckwrksAppJS :: JStat
clckwrksAppJS = [jmacro|
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

    clckwrksApp.controller('DemoAppCtrl', ['$scope', '$http',function($scope, $http) {
      $scope.message = '';

      $scope.callRestricted = function (url) {
        $http({url: url, method: 'GET'}).
        success(function (datum, status, headers, config) {
          $scope.message = datum.name;
        }).
        error(function (datum, status, headers, config) {
          alert(datum);
        });
      };
    }]);
  }
 |]
