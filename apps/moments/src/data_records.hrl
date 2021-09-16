-record(moment, {moment_id,
                 name,
                 interval,
                 excl_days,
                 excl_time,
                 admin,
                 private}).

-record(user, {user_id,
              name}).

-record(device, {device_id,
                name}).

-record(follows, {user,
                 moment}).

-record(admin_of, {user,
                 moment}).

-record(owns, {user,
         device}).
