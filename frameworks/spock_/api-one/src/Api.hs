module Api where 

import Api.User

app :: Api
app = do
  getUsers
  getUser
  postUser
  deleteUser
