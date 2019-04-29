module Api (api) where

import           Api.User
import           Api.Root
import           ApiTypes (Api)

api :: Api
api = do
  getRoot
  getUsers
  getUser
  postUser
  deleteUser
