(function(){
    var app = angular.module('pureApp',[]);
    app.controller("MainController", ['$scope', function($scope){
        this.messages = [];
        this.saveMessage = function(){
         this.messages.push({body: $scope.body, 
            to: $scope.to});
         $scope.body = $scope.to = null;
         console.log(this.messages);
        };
        
    }]);
    
    
})();
