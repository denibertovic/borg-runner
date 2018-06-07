# borg-runner

| Master |
| -------|
| [![master](https://travis-ci.org/denibertovic/borg-runner.svg?branch=master)](https://travis-ci.org/denibertovic/borg-runner) |

A helper tool for running borg backups

## Example config file

```yaml
# NOTE: globs must be in quotes because YAML
excludes:
- /home/myuser/.cache
- "*.pyc"
- .stack-work
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

## Running

```bash
sudo borg-runner --debug -c ~/.borg-runner-config.yaml
```

`NOTE`: The idea is to add this as a cronjob
