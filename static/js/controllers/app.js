(function(){
    var app = angular.module('pureApp',[]);
    app.controller("MainController", ['$scope','$http', function($scope, $http){
        var env = this;
        $http.defaults.headers.post["Content-Type"] = "application/x-www-form-urlencoded";
        this.messages = [];
        env.ciphertext;
        env.user;
        env.destinationPublicKey; 
        env.pkint;
        env.foundUser = false;
        // Gets user information from server
        this.getUserInfo = function(){
                 $http.get('http://localhost:3000/api/user/fullinfo/' + $scope.to).success(function(data){                 
                 env.user = data
                 env.foundUser = true;
         });
        };
        
        //Sends an encrypted message
        this.saveMessage = function(){ 
        
          
          envsave = this;       
          this.user;
          this.now = Date.now();
          this.message = { 
                            to: env.user.login,
                            from: 'renny', //TODO: change value with username in session
                            body: $scope.body,                             
                            createdAt: this.now
                          }
          this.toAuth;

          
          // Get destination public key
          $http.get('http://localhost:3000/api/publickey/' + this.user.login).success(function(pkdata){          

            //Retrieve public_n value, decode it from Base16 and create a new BigInt             


            var public_e = pkdata.public_e.toString();
            var public_n = forge.util.decode64(pkdata.public_n);
            var exponent = new forge.jsbn.BigInteger(public_e,10);
            env.pkint = new forge.jsbn.BigInteger(public_n, 10);
            //Recreate Public Key 
            env.destinationPublicKey = forge.pki.setRsaPublicKey(env.pkint, exponent);

            //encrypt message ... 
            env.ciphertext = env.destinationPublicKey.encrypt(forge.util.createBuffer(env.message.body));
            env.toAuth = env.ciphertext + "+" +  env.message.to + "+" + env.message.from + "+" + env.message.createdAt;
            env.ciphertext64 = forge.util.encode64(env.ciphertext);
            
            //... then, sign. 
            
            // Save message
            this.datapacket = {
                     to: env.user.login,
                     from: 'renny', //TODO: change value with username in session
                     body: env.ciphertext64,
                     createdAt: env.message.createdAt,
                     authMessage: env.toAuth //TODO: Authenticate this string
            };
            
            $http.post('http://localhost:3000/api/message/send', this.datapacket).
            success(function(data, status, headers, config) {
              env.messages.push(env.message);
              $scope.body = $scope.to = null;
              env.foundUser = false;
            });
          });                                 
        };
        // Returns true or false whether user is found 
        this.userIsFound = function() {
          return(this.foundUser);
        }
        
    }]);
    
    
})();
