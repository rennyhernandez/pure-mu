(function(){
    var app = angular.module('pureApp',[]);
    app.controller("MainController", ['$scope','$http', function($scope, $http){
        var env = this;
        $http.defaults.headers.post["Content-Type"] = "application/x-www-form-urlencoded";
        this.messages = [];
        this.saveMessage = function(){
        
        this.message = { body: $scope.body, 
                         to: $scope.to 
                        }
         $http.post('http://localhost:3000/api/message/send', this.message).
         success(function(data, status, headers, config) {
           env.messages.push(env.message);
           $scope.body = $scope.to = null;

        });
         
        };
        
    }]);
    
    
})();
