(function(){
    var app = angular.module('pureApp',[]);
    app.controller("MainController", ['$scope','$http', function($scope, $http){
        var env = this;
        $http.defaults.headers.post["Content-Type"] = "application/x-www-form-urlencoded";
        this.messages = [];
        env.ciphertext;
        env.destinationPublicKey; 
        env.pkint;
        
        this.getUserInfo = function(){
                 $http.get('http://localhost:3000/api/user/fullinfo/' + $scope.to).success(function(data){                 
                 env.user = data
         });
        };
        this.saveMessage = function(){        
          this.user;
          this.message = { 
                            to: this.user.login,
                            from: 'renny', //TODO: change value with username in session
                            body: $scope.body,                             
                            createdAt: Date.now()
                          }
          
          // Get destination public key
          $http.get('http://localhost:3000/api/publickey/' + this.user.login).success(function(pkdata){          

            //Retrieve public_n value, decode it from Base16 and create a new BigInt             
            env.pkint = new forge.jsbn.BigInteger(pkdata.public_n, 16);
            var public_e = pkdata.public_e.toString();
            var exponent = new forge.jsbn.BigInteger(public_e,10);

            console.log(exponent);
            env.destinationPublicKey = forge.pki.setRsaPublicKey(env.pkint, exponent);
            console.log(env.pkint.toString());
            console.log(env.destinationPublicKey);
            //encrypt message 
            env.ciphertext = env.destinationPublicKey.encrypt(forge.util.createBuffer(env.message));
            console.log(env.ciphertext);
            // Save message to Server
            $http.post('http://localhost:3000/api/message/send', this.message).
            success(function(data, status, headers, config) {
              env.messages.push(env.message);
              $scope.body = $scope.to = null;
            });
          });                         

          
          

         
        };
        
    }]);
    
    
})();
