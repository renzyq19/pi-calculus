module Types where

data ChannelType = Internal
                 | String
                 | HTTP
                 deriving (Eq, Show, Read)
