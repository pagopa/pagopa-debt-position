- [Load tests for GPD project](#load-tests-for-gpd-project)
  - [01. Debtor position creation](#01-debtor-position-creation)
# Load tests for GPD project

This is a set of [k6](https://k6.io) load tests related to the GPD (_Gestione Posizioni Debitorie_) initiative.


## 01. Debtor position creation

```
k6 run gpd/load-test/src/create_debt_postion.js
```
