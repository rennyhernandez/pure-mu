import Crypto.Cipher.AES
import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString as B
import Crypto.Random
import Crypto.PBKDF.ByteString (sha256PBKDF2)
import qualified Data.ByteString.Base64 as B64 -- RFC 4648 compliant
import qualified Data.ByteString.Base16 as B16
import Crypto.Hash.SHA256 (hash)

encrypt = do
  message <- BC.getLine     
  password <- BC.getLine  
  
  -- 
  -- Generate key from password 
  --
     

  let  (salt, _) = (B16.decode . BC.pack) "403995189c1e622d39e2b4466883c5a6a932878d33ab045368c45f09b87fc294" -- in hex
  
  let key = sha256PBKDF2 password salt 1000 32


  -- Begin AES CTR Encryption
  -- Generate Initialization Vector for CTR Encryption
  g <- newGenIO :: IO SystemRandom
  let eitherRand = genBytes 16 g
  (iv, g') <- case eitherRand of 
    Left _ ->  error "Could not generate random bytes"
    Right t -> return t
  let context = initAES key
  let ciphertext = encryptCTR context iv message
  

  BC.putStrLn $ B16.encode ciphertext
  BC.putStrLn $ B16.encode iv
  
--  BC.putStrLn $ B16.encode tag
  {-
  let eitherC = (B64.decode . BC.pack) ciphertext
  c <- case eitherC of 
    Left _ -> error "Could not read ciphertext"
    Right c'  -> return c'
   -}
--  let plaintext = BC.unpack $ decryptCTR context iv c

-- putStrLn plaintext
  
  
decrypt = do

  cin <- BC.getLine  -- in hex base
  password <- BC.getLine  -- plaintext password
  nonce <- BC.getLine

  let (ciphertext, _) = B16.decode cin
  let (iv, _) = B16.decode nonce
  
  

  
  -- 
  -- Generate key from password 
  --

  let (salt, _) = (B16.decode . BC.pack) "403995189c1e622d39e2b4466883c5a6a932878d33ab045368c45f09b87fc294" -- in hex
  let key = sha256PBKDF2 password salt 1000 32
  
  -- 
  -- Begin AES CTR Decryption
  -- 
  -- Initialize key
  let context = initAES key
  -- In this case, the message is both encrypted and authenticated. 
  let message = decryptCTR context iv ciphertext
  
  BC.putStrLn $ message
--  fmap BC.putStrLn tag

  
