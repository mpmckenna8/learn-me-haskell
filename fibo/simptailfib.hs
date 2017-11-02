-- tail call fibonacci attempt

addfib index target x y = if index == target
                            then x + y;
		                     else
                         addfib (index+1) target y (x + y)

tfib 0 = 0;
tfib 1 = 1;
tfib 2 = 1;

tfib n = addfib 3 n 1 2 


