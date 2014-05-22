module Types where

data ChannelType = Internal
                 | Std
                 | HTTP
                 | String
                 deriving (Eq, Show, Read)
