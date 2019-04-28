module Api where

import           Api.User
import           ApiTypes

app :: Api
app = do
  getUsers
  getUser
  postUser
  deleteUser
