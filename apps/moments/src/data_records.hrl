-type utc_time() :: integer().
-record(moment, {moment_id :: string() | '_',
                 name :: string() | '_',
                 next_moment :: utc_time() | '_',
                 interval :: integer() | '_',
                 excl_days :: list() | '_',
                 excl_time :: list() | '_',
                 private :: boolean() | '_'}).

-record(user, {user_id :: string() | '_',
              name :: string() | '_'}).

-record(device, {device_id :: string() | '_',
                name :: string() | '_'}).

-record(follows, {user :: string() | '_',
                 moment :: string() | '_'}).

-record(admin_of, {user :: string() | '_',
                 moment :: string() | '_'}).

-record(owns, {user :: string() | '_',
         device :: string() | '_'}).
