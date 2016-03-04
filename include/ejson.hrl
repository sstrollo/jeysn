
-record(json_error, { code :: atom()
                      , message :: string()
                      , token  :: undefined | ejson:json_token()
                      , name   :: string()
                      , offset :: non_neg_integer()
                      , line   :: pos_integer()
                      , column :: pos_integer()
                    }).

