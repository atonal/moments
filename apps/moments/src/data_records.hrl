-type utc_time() :: integer().
-type unique_id() :: non_neg_integer().

-type id_type() :: moment | user | device.

-type moment_id() :: unique_id().
-type moment_name() :: string().
-type next_moment() :: utc_time().
-type interval() :: debug | hourly | daily | weekly | monthly | yearly.
-type excl_days() :: list().
-type excl_time() :: list().
-type private() :: boolean().
-record(moment, {moment_id :: moment_id() | '_',
                 name :: moment_name() | '_',
                 next_moment :: next_moment() | '_',
                 interval :: interval() | '_',
                 excl_days :: excl_days() | '_',
                 excl_time :: excl_time() | '_',
                 private :: private() | '_'}).
-type moment() :: #moment{
                     moment_id :: moment_id(),
                     name :: moment_name(),
                     next_moment :: next_moment(),
                     interval :: interval(),
                     excl_days :: excl_days(),
                     excl_time :: excl_time(),
                     private :: private()}.

-type user_id() :: unique_id().
-type user_name() :: string().
-record(user, {user_id :: user_id() | '_',
              name :: user_name() | '_'}).

-type device_id() :: unique_id().
-type device_name() :: string().
-record(device, {device_id :: device_id() | '_',
                name :: device_name() | '_'}).

-record(follows, {user :: user_id() | '_',
                 moment :: moment_id() | '_'}).

-record(admin_of, {user :: user_id() | '_',
                 moment :: moment_id() | '_'}).

-record(owns, {user :: user_id() | '_',
         device :: device_id() | '_'}).

-record(table_id, {table_name :: atom(),
                   last_id :: non_neg_integer()}).
