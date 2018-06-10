# borg-runner

| Master |
| -------|
| [![master](https://travis-ci.org/denibertovic/borg-runner.svg?branch=master)](https://travis-ci.org/denibertovic/borg-runner) |

A helper tool for running borg backups

## Limitations

* The tool currently assumes that you will mount the backup volume from your local NAS device using
  NFS. It also assumes that it will unmount the volume at the end using `umount`.
* The tool assumes that you will only run the backups when connected to you home (or other designated) wireless
  network.
* You have to install `nmcli` since that's what borg-runner uses to check what SSID you're currently
  connected to.
* Because of the above running the backups over ethernet is currently not supported.
* You have to have `zlib` installed since we used that for compressing the backups. If you installed
  `borgbackup` using `apt` you likely don't have to worry about this.
* The tool currently assumes that the borg backup repo is encrypted using a `passphrase`. Other encryption
  methods (ie. keys) might be supported in the future.

## Example config file

```yaml
# NOTE: globs must be in quotes because YAML
excludes:
- /home/myuser/.cache
- "*.pyc"
- .stack-work
- .stack
- node_modules
- bower_components
- /etc/shadow
- /etc/shadow-

includes:
- /home/myuser
- /etc

mount_command: /home/myuser/dotfiles/scripts/mount_backups.sh
mount_path: /media/backups
repo_name: myRepo

password: superSecretPw

# NOTE: currently only wifi support since we use nmcli to identify when we're connected
# to the Home wifi SSID
network_name: MyHomeWifi
network_device: someIfaceName
```

`NOTE`: The `mount_command`, in my case is just a simple bash script like this:

```bash
#!/bin/bash

mount 192.168.0.43:/volume1/backups /media/varys/backups

```

Save this file in /etc/borg-runner.conf make sure to set the correct permissions:


```bash
chown root:root /etc/borg-runner.yaml
chmod 600 /etc/borg-runner.yaml
```

## Running

Borg runner needs root permissions to run. This is mostly because it will need to mount `NFS` volumes and the like, but
also because if you want to backup a folder like `/etc` you have to run the backup as root.

```bash
sudo borg-runner --debug -c /etc/borg-runner.yaml
```

`NOTE`: The idea is to add this as a cronjob and run it how often you'd like.

Example crontab config:

```
30 19-23 * * * borg-runner --debug -c /etc/borg-runner.yaml 2>&1 | logger -t borgrunner
```

This will attempt to run the backups every 30 minutes between the hours 19-23. It will only actually run the
backup if I'm connected to my home wifi (where I am actually able to mount my NAS drive). If I'm not connected to my
home wifi it will just exit.

The backup usually finishes for me in 30 minutes so a second run will just see that there already is a backup for today
and will not do another one. If for some reason a backup is taking longer to finish and second cron is triggered borg
will simply that there is a lock file present and will fail to run the backup anyway.
