
# coding: utf-8

# In[2]:

def autoRst():

    import subprocess
    import io_manip

    choice = 1

    #keeping track of each step
    filePath = ''
    default = True
    first = True
    legs = []
    time, n_util, p_util, potential, uocp, curr, temp, heatgen = [], [], [], [], [], [], [], []
    
    #find path to files
    response = str(input('Are dualfoil files in this directory (Y or N)?  '))
    response = response.upper()
    if response == 'N':
        filePath = str(input('\nPlease provide the path to these files:  '))
        if not filePath.endswith('/'):
            filePath += '/'
        default = False

    #copy input data so it can be reverted back after simulation
    if default == True:
        subprocess.call('cp dualfoil5.in inputCopy.in', shell=True)
    else:
        subprocess.call('cd %s && cp dualfoil5.in inputCopy.in' %(filePath), shell=True)

    #operational loop
    while choice != 2:
        #get the step
        while True:
            if first == False:
                choice = int(input('Possible actions: (1) Continue with a new step (2) Quit and gather data \nChoice?  '))
                choice = int(choice)

            if choice == 1 :
                comment = ''
                comment = str(input('What does this step do? (brief comment): '))
                line = comment
                while True:
                    try:
                        cmd = ''
                        cmd = str(input('\nEnter new command line to be executed.\nFormat (one space separator): cu(i) tt(i) mc(i) vcutLow vcutHigh\n'))
                        print(cmd)
                        line = cmd + "  !" + line + '\n'
                        cmd = cmd.split(' ')
                        cu, tt, mc, vcutL, vcutH = float(cmd[0]), float(cmd[1]), int(cmd[2]), float(cmd[3]), float(cmd[4])
                        break
                    except ValueError:
                        print('\nImproper command format. Please try again')
                    except IndexError:
                        print('\nNot enough variables detected. Please try again')

                #update input file and steplist
                if first == True:
                    io_manip.first_leg(comment, cu, tt, mc, vcutL, vcutH)
                    first = False
                else:
                    io_manip.add_new_leg(comment, cu, tt, mc, vcutL, vcutH)
                    
                legs.append(line)
                break
            if choice == 2:
                break

            #must be incorrect entry if we made it here
            print('Error: Please select an available option.')
        
        
        if choice == 1:
            #run dualfoil
            if default == True:
                subprocess.call('./dualfoil', shell=True)
            else:
                subprocess.call('cd %s && ./dualfoil' %(filePath), shell=True)

            #track the main output
            a, b, c, d, e, f, g, h = io_manip.extract_main_output('%sdualfoil5.out' %(filePath))
            time +=a 
            n_util += b
            p_util += c
            potential += d
            uocp += e
            curr += f
            temp += g
            heatgen += h

        

    #gather final output; write to an output file
    output = [time, n_util, p_util, potential, uocp, curr, temp, heatgen]
    print(len(output))
    if default:
        subprocess.call('date > combinedOutput.out', shell=True)
    else:
        subprocess.call('date > %scombinedOutput.out' %(filePath), shell=True)
        
    with open('%scombinedOutput.out' %(filePath), 'a') as outFile:
        outFile.write('\nMain Output data\n\n')
        outFile.write('     Time   N_util   P_util Potential    Uocp     Curr    Temp    Heatgen\n')
        outFile.write('     (min)    x         y      (v)        (v)    (A/m2)    (C)     (W/m2)\n\n')
        for i in range(len(time)):
            for j in range(len(output)):
                outFile.write(str(output[j][i]).rjust(9))
                outFile.write(',')
                if j == (len(output)-1):
                    outFile.write('\n')

    #restore input; write legs to a file
    if default:
        subprocess.call('mv inputCopy.in dualfoil5.in', shell=True)
        subprocess.call('date > legs.dat', shell=True)
        with open('legs.dat', 'a') as legsFile:
            for string in legs:
                legsFile.write(string)
    else:
        subprocess.call('cd %s && mv inputCopy.in dualfoil5.in' %(filePath), shell=True)
        subprocess.call('date > %slegs.dat' %(filePath), shell=True)
        with open('%slegs.dat' %(filePath), 'a') as legsFile:
            for string in legs:
                legsFile.write(string)
            

autoRst()