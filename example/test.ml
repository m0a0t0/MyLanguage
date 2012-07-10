import lib
a = [1,2,3,4]
foreach (n in a) {
    a += n
}
for (i=0;i<=9;i+=1) {
    a += i
}
print(a)
a[0] = 2
print(a)
