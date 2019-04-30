module Api (api) where

import           Api.Root
import           Api.User
import           ApiTypes (Api)

api :: Api
api = do
  getRoot
  getUsers
  getUser
  postUser
  deleteUser
