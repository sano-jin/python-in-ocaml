def timer ():
    time = 0
    def tick ():
#        nonlocal time
        time = time + 1
        print (time)
    return tick
clock = timer ()
clock ()
clock ()
clock ()
# ---> 1 2 3
