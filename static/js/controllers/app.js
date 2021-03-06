(function(){
    var app = angular.module('pureApp',[]);
    app.controller("MainController", ['$scope','$http', function($scope, $http){
        var env = this;
        $http.defaults.headers.post["Content-Type"] = "application/x-www-form-urlencoded";
        this.messages = [];
        env.ciphertext;
        env.user;
        env.loggedUser = 'renny';
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
                            from: env.loggedUser, //TODO: change value with username in session
                            body: $scope.body,                             
                            createdAt: this.now
                          }
          this.toAuth;

          
          // Get destination public key
          $http.get('http://localhost:3000/api/publickey/' + this.user.login).success(function(data){          
              
            //Retrieve public_n value, decode it from Base64 and create a new BigInt             



            var jsn = forge.util.decode64(data.public_n);
            var exponent = new forge.jsbn.BigInteger(data.public_e.toString(), 10);
            var public_n = new forge.jsbn.BigInteger(jsn, 10);
            //Recreate Public Key 
            var destinationPublicKey = forge.pki.setRsaPublicKey(public_n, exponent);

            //encrypt message ... 
            env.ciphertext = destinationPublicKey.encrypt(forge.util.createBuffer(env.message.body).getBytes());
            env.toAuth = env.ciphertext + "+" +  env.message.to + "+" + env.message.from + "+" + env.message.createdAt;
            env.ciphertext64 = forge.util.encode64(env.ciphertext);

            //TODO:... then, sign. 
            console.log(env.ciphertext64);
            // Save message
            this.datapacket = {
                     to: env.user.login,
                     from: env.loggedUser, //TODO: change value with username in session
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
        };      
    }]);
    
  app.controller("DecryptController", ['$scope', '$http', function($scope, $http){
    this.loggedOnUser = "saray"; // TODO: Logged in user 
    this.password;
    this.decryptMessage = function(){
      console.log($(".message").html());
    };
    
    this.isChanging = function(){
      var messages = $(".message");

      if($scope.password == "cambiame"){ //TODO: Must be modified to any password
        $http.get('http://localhost:3000/api/privatekey/' + this.loggedOnUser).success(function(data){
          var exponent = data.private_pub.public_e;
          var private_d = new forge.jsbn.BigInteger(forge.util.decode64(data.private_d), 10);
          var public_n = new forge.jsbn.BigInteger(forge.util.decode64(data.private_pub.public_n), 10);
          
          var private_key = forge.pki.setRsaPrivateKey(public_n, exponent, private_d);
          
          messages.each(function(){
            var ciphertext = forge.util.decode64($(this).html());
            console.log(ciphertext);
            console.log(private_key);
            var deciphered = private_key.decrypt(ciphertext);
            $(this).text(deciphered);
            
          });
        });
   
      }
     
    };
  }]);
    
    
})();
