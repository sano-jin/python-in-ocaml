def timer ():
    def tick ():
        nonlocal time
        time = time + 1
        print (time)
    time = 0
    return tick
clock = timer ()
clock ()
clock ()
clock ()
