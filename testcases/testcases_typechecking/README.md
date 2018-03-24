
t12.tig # success

/*CallExp*/
print("hello, world")

t13.tig # fail

/*callexp: more parameter than expected*/
print("hello, world", "v2")

t14.tig # fail

/*callexp: no param*/
print()

t15.tig # fail
/*callexp: wrong param types*/
print(5)
