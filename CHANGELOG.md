# Changelog


## 0.0.3

- Execute attr-machine hook prior to selection mechansim
- Fix chmod in rexec
- Don't do println within a (core/with-machines) iteration (as it's exec'd in parallel), print resultant seq

