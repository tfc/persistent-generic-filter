# persistent-generic-filter

Program output / user interaction:

```bash
The following people exist in the DB:
Person {personName = "john", personAge = 10}
Person {personName = "tom", personAge = 10}
Person {personName = "jane", personAge = 20}
Person {personName = "john", personAge = 30}

Please enter a query. Examples:
    age > 12 and name = john
    name = jane or (name = john and age > 10)

The following fields and operators are available:
Field "age", operators: = < >
Field "name", operators: =

query> age > 12 and name = john
Results of query "age > 12 and name = john":
Person {personName = "john", personAge = 30}

query> age > 12 and name = "jane"
Results of query "age > 12 and name = "jane"":
Person {personName = "jane", personAge = 20}

query> name = jane or (name = john and age > 10)
Results of query "name = jane or (name = john and age > 10)":
Person {personName = "jane", personAge = 20}
Person {personName = "john", personAge = 30}
```
