module Api where

import           Api.User
import           Api.Root
import           ApiTypes

app :: Api
app = do
  getRoot
  getUsers
  getUser
  postUser
  deleteUser
