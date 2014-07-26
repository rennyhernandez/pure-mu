(function(){
    var app = angular.module('pureApp',[]);
    app.controller("MainController", ['$scope','$http', function($scope, $http){
        var env = this;
        $http.defaults.headers.post["Content-Type"] = "application/x-www-form-urlencoded";
        this.messages = [];
        this.getUserInfo = function(){
                 $http.get('http://localhost:3000/api/user/fullinfo/' + $scope.to).success(function(data){
                 env.user = data
         });
        };
        this.saveMessage = function(){
        
          this.message = { 
                            to: this.user.login,
                            from: 'renny', //TODO: change value with username in session
                            body: $scope.body,                             
                            createdAt: Date.now()
                          }
          this.user;

           //Save message to Server
           $http.post('http://localhost:3000/api/message/send', this.message).
           success(function(data, status, headers, config) {
             env.messages.push(env.message);
             $scope.body = $scope.to = null;

          });
         
        };
        
    }]);
    
    
})();
