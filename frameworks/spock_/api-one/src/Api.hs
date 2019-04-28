module Api where 

import Web.Spock
import Api.User
import Models

app :: Api
app = do
  getUsers
  getUser
  postUser
  deleteUser
