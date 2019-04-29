module Api where

import           Api.User
import           Api.Root
import           ApiTypes

api :: Api
api = do
  getRoot
  getUsers
  getUser
  postUser
  deleteUser
