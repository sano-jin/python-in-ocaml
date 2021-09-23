try:
    print('enter try statement')
    raise Exception()
    print('exit try statement')

except Exception as inst:
    print(inst.__class__.__name__)

                         
