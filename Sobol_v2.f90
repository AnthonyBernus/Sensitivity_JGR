


program Sobol





use netcdf


!--------------------------
!
! Declaration
!
!--------------------------
 IMPLICIT NONE

! Indication on the number of simulation
!-------------------------
INTEGER :: 			N		, & ! Index on the ID of the simulation
				j		, & ! Size of the sample
				i		, & ! Size of the sample
				t		    ! Size of the sample


CHARACTER (len = 4) ::		id_A		    ! list of the ID number 
CHARACTER (len = 4) ::		id_B		    ! list of the ID number 
CHARACTER (LEN = 10) ::		N_str
! Indication on the access of the files 
!-------------------------
CHARACTER (LEN = 250) ::	path_in_A	, & ! Path of the folder of the A matrix
				path_in_A_id	, & ! Path of the netcdf file inside the folder A matrix
				path_in_B_id	, & ! Path of the netcdf file inside the folder B matrix
				path_in_B	, & ! Path of the folder of the B matrix
				path_in_BA	, & ! Path of the folder of the BA matrix
				path_in_BA_id	, & ! Path of the netcdf file inside the folder BA
				path_in_AB	, & ! Path of the folder of the BA matrix
				path_in_AB_id	, & ! Path of the netcdf file inside the folder BA
				path_output	, &
				Y_prof 		, &
				Y_climat	, &
				path_tree

! ID of the netcdf file treated
!-------------------------

INTEGER :: 			stat 		    ! Check up the well functionment of the netcdf function task


INTEGER ::			ncid_read_A 	, & ! ID for the current studied netcdf file for the A matrix
				ncid_read_B	, & ! ID for the current studied netcdf file for the B matrix	
				ncid_read_BA 	, & ! ID for the current studied netcdf file for the matrix mix the information from the matrix A and B	
				ncid_read_AB 	    ! ID for the current studied netcdf file for the matrix mix the information from the matrix A and B	




! Id to get the acces of the output variable in the netcdf file
!-------------------------
INTEGER ::			varid_A 	, & ! ID of the variable in the A matrix
				varid_B		, & ! ID of the variable in the B matrix	
				varid_BA 	, & ! ID of the variable in the BA matrix
				varid_AB 	    ! ID of the variable in the AB matrix

CHARACTER (LEN = 10) ::	Y_name 		     	    ! Name of the studied variable



! The input of the program
!-------------------------



! The output model variable to calcul the Sobol indice 
REAL (KIND=8), DIMENSION(17520) ::	Y_B		, & ! Model output of the matrix B
					Y_A		, & ! Model output of the matrix A
					Y_BA		, & ! Model output of the matrix BA
					Y_AB		    ! Model output of the matrix AB

INTEGER :: 			D		    ! Number of dimension		


CHARACTER (len = 4) ::		dimension	    ! list of the ID number 


! Time parametrisation
INTEGER ::			Nbtime		, & ! Number of time step during the year
				start		, & ! THe first studied year
				nb_year		, & ! Number of studied year
				year
CHARACTER (len = 4) ::		str_year 

INTEGER, DIMENSION(1) ::	list_read	    ! List to index the time to read the rigth information in the netcdf file



! The output from this program
!-----------------------------

REAL (KIND=8), DIMENSION(17520,7) ::	var_param	, & ! Standard deviation to calculate the indice of Sobol (time step maximum 30min, Dimension max 5)
				sobol_indice	, &
				var_tot_param	, &
				sobol_indice_tot

REAL (KIND=8), DIMENSION(17520) ::	var_tot		, & ! Total Standard deviation to calculate the indice of Sobol (time step maximum 30min)
					mean_f		, &
					VAR_n		, &
					MEAN_n		, &
					VAR		, &
					MEAN		, &
					STD		


REAL (KIND=8), DIMENSION(9) ::	VAR1


INTEGER :: SKIP


SKIP=0


!--------------------------
! Parameters Initialisation 
!--------------------------


N=4000		! Initialisation of size of the sample
D=7		! Initialisation of the number of dimension



!--------------------------
! Choice of the output parameter to work out the Sobol indice  
!--------------------------

CALL GET_ENVIRONMENT_VARIABLE("NAME", Y_name)
CALL GET_ENVIRONMENT_VARIABLE("PROF", Y_prof)
CALL GET_ENVIRONMENT_VARIABLE("CLIMAT", Y_climat)
CALL GET_ENVIRONMENT_VARIABLE("TREE", path_tree)


!--------------------------
! Time Period Initialisation 
!--------------------------

start=2000
nb_year=5
Nbtime=17520




!--------------------------
! Path to get the matrix
!--------------------------


path_in_A = trim(path_tree)//"PROFONDEUR_"//trim(Y_prof)//"/"//"CLIMAT_"//trim(Y_climat)//"/"//"MATRICE_A/"
path_in_B = trim(path_tree)//"PROFONDEUR_"//trim(Y_prof)//"/"//"CLIMAT_"//trim(Y_climat)//"/"//"MATRICE_B/"
path_in_BA = trim(path_tree)//"PROFONDEUR_"//trim(Y_prof)//"/"//"CLIMAT_"//trim(Y_climat)//"/"//"MATRICE_B"
path_in_AB = trim(path_tree)//"PROFONDEUR_"//trim(Y_prof)//"/"//"CLIMAT_"//trim(Y_climat)//"/"//"MATRICE_A"




!--------------------------
! Calcul of each Si
!--------------------------




do year = start, start+nb_year-1

	write (str_year, 101)  year
101	format(I4)




!==============================================================================
!==============================================================================
!==============================================================================
!==============================================================================
	! Make the normalised outputs (allow to make the convergence) 
	!-----------------------------------------------



	VAR=0
	MEAN=0
	VAR_n=0
	MEAN_n=0

	do j=1,N
		write (id_A, 200) j+SKIP		! Transform the format ID from integer to string
200		format(I4)
		


		! Get the path of the simulation from the matrix A and B
		path_in_A_id=trim(path_in_A)//"SIMULATION_ID_"// trim(adjustl(id_A))//"/RESULTAT/output"// str_year//".nc"
		path_in_B_id=trim(path_in_B)//"SIMULATION_ID_"// trim(adjustl(id_A))//"/RESULTAT/output"//str_year  //".nc"


		! Open the netcdf file from the matrix A and B with the treated ID
		stat =nf90_open(path_in_A_id, NF90_NOWRITE, ncid_read_A)
		stat =nf90_inq_varid(ncid_read_A, Y_name , varid_A)

		stat = nf90_open(path_in_B_id, NF90_NOWRITE, ncid_read_B)
		stat = nf90_inq_varid(ncid_read_B, Y_name , varid_B)




		! Time loop to get all the output model variable across the studied year
		do t=1,Nbtime

			list_read(1)=t
			stat =nf90_get_var(ncid_read_A, varid_A, Y_A(t), start=list_read)
			stat =nf90_get_var(ncid_read_B, varid_B, Y_B(t), start=list_read)

		end do


		! Close the two netcdf file
		stat = NF90_CLOSE(ncid_read_A)
		stat = NF90_CLOSE(ncid_read_B)

		do i = 1, D


			! Disk access of the coupling matrixs A and B
			!-----------------------------------
			write (dimension, 106) i	! Transform the format dimension from integer to string
106			format(I4)



			! Get the path of the simulation from the coupling matrixs A and B
			path_in_BA_id = trim(path_in_BA) // trim(adjustl(dimension)) // "/SIMULATION_ID_"// trim(adjustl(id_A))//"/RESULTAT/output"//str_year //".nc"
			path_in_AB_id = trim(path_in_AB) // trim(adjustl(dimension)) // "/SIMULATION_ID_"// trim(adjustl(id_A))//"/RESULTAT/output"//str_year //".nc"


			! Treated the matrix BA (B1, B2, ...)
			!------------------------------------

			! Open the netcdf file from the matrix A and B with the treated ID
			stat = nf90_open( path_in_BA_id, NF90_NOWRITE, ncid_read_BA)
			stat = nf90_inq_varid(ncid_read_BA, Y_name , varid_BA)


			! Time loop to get all the output model variable across the studied year
			do t=1,Nbtime

				list_read(1)=t
				stat =nf90_get_var(ncid_read_BA, varid_BA, Y_BA(t), start=list_read)
	
			end do
	


	
			! Close the two netcdf file
			stat = NF90_CLOSE(ncid_read_BA)



			! Treated the matrix AB (A1, A2, ...)
			!------------------------------------


			! Open the netcdf file from the matrix A and B with the treated ID
			stat = nf90_open( path_in_AB_id, NF90_NOWRITE, ncid_read_AB)
			stat = nf90_inq_varid(ncid_read_AB, Y_name , varid_AB)

			
			! Time loop to get all the output model variable across the studied year
			do t=1,Nbtime

				list_read(1)=t
				stat =nf90_get_var(ncid_read_AB, varid_AB, Y_AB(t), start=list_read)

			end do
		
			! Close the two netcdf file
			stat = NF90_CLOSE(ncid_read_AB)

			do t=1, Nbtime
				VAR_n(t)=VAR_n(t)+Y_AB(t)**2+Y_BA(t)**2 
				MEAN_n(t)=MEAN_n(t)+Y_AB(t)+Y_BA(t)

			end do

	

		end do


		! Calcul mean and standard deviation  
		!-----------------------------------


		do t=1, Nbtime
			VAR_n(t)=VAR_n(t)+Y_A(t)**2+Y_B(t)**2 
			MEAN_n(t)=MEAN_n(t)+Y_A(t)+Y_B(t)
			VAR(t)=VAR(t)+Y_A(t)**2+Y_B(t)**2 
			MEAN(t)=MEAN(t)+Y_A(t)+Y_B(t)

		end do



	end do

	! Mean calcul

	do t=1, Nbtime
		MEAN_n(t)=MEAN_n(t)/(2*D+2)/N
		MEAN(t)=MEAN(t)/2/N

	end do


	! Standard deviation calcul

	do t=1, Nbtime
		VAR_n(t)=VAR_n(t)/(2*D+2)/N-(MEAN_n(t))**2
		VAR(t)=VAR(t)/2/N-(MEAN(t))**2
	print *, VAR(t)
	end do


!==============================================================================
!==============================================================================
!==============================================================================


	!-----------------------------------------------
	! Initialisation to 0 of the statistic variables
	!-----------------------------------------------

	do i = 1, D
		var_param(:,i)=0
		var_tot_param(:,i)=0

	end do

	var_tot=0
	mean_f=0

	!-----------------------------------------------
	! Loop to treat all the N indepedant simulations
	!-----------------------------------------------

	do j=1,N


		! Convert id in string type


		! Disk access of the matrice A and B
		!-----------------------------------


		write (id_A, 201) j+SKIP		! Transform the format ID from integer to string
201		format(I4)
		

		! Get the path of the simulation from the matrix A and B
		path_in_A_id=trim(path_in_A)//"SIMULATION_ID_"// trim(adjustl(id_A))//"/RESULTAT/output"// str_year//".nc"
		path_in_B_id=trim(path_in_B)//"SIMULATION_ID_"// trim(adjustl(id_A))//"/RESULTAT/output"//str_year  //".nc"


		! Open the netcdf file from the matrix A and B with the treated ID
		stat =nf90_open(path_in_A_id, NF90_NOWRITE, ncid_read_A)
		stat =nf90_inq_varid(ncid_read_A, Y_name , varid_A)

		stat = nf90_open(path_in_B_id, NF90_NOWRITE, ncid_read_B)	
		stat = nf90_inq_varid(ncid_read_B, Y_name , varid_B)


		! Time loop to get all the output model variable across the studied year
		do t=1,Nbtime

			list_read(1)=t
			stat =nf90_get_var(ncid_read_A, varid_A, Y_A(t), start=list_read)
			stat =nf90_get_var(ncid_read_B, varid_B, Y_B(t), start=list_read)

		end do


		! Close the two netcdf file
		stat = NF90_CLOSE(ncid_read_A)
		stat = NF90_CLOSE(ncid_read_B)



		! Loop to access to the output model varaible across the studied year from the coupling matrixs A and B (A1, A2,..., B1, B2, ....)
		do i = 1, D


			! Disk access of the coupling matrixs A and B
			!-----------------------------------
			write (dimension, 102) i	! Transform the format dimension from integer to string
102			format(I4)



			! Get the path of the simulation from the coupling matrixs A and B
			path_in_BA_id = trim(path_in_BA) // trim(adjustl(dimension)) // "/SIMULATION_ID_"// trim(adjustl(id_A))//"/RESULTAT/output"//str_year  //".nc"
			path_in_AB_id = trim(path_in_AB) // trim(adjustl(dimension)) // "/SIMULATION_ID_"// trim(adjustl(id_A))//"/RESULTAT/output"//str_year  //".nc"


			! Treated the matrix BA (B1, B2, ...)
			!------------------------------------


			! Open the netcdf file from the matrix A and B with the treated ID
			stat = nf90_open( path_in_BA_id, NF90_NOWRITE, ncid_read_BA)
			stat = nf90_inq_varid(ncid_read_BA, Y_name , varid_BA)


			! Time loop to get all the output model variable across the studied year
			do t = 1, Nbtime

				list_read(1)=t
				stat = nf90_get_var(ncid_read_BA, varid_BA, Y_BA(t), start=list_read)

			end do
	


	
			! Close the two netcdf file
			stat = NF90_CLOSE(ncid_read_BA)


	

			! Treated the matrix AB (A1, A2, ...)
			!------------------------------------


			! Open the netcdf file from the matrix A and B with the treated ID
			stat = nf90_open( path_in_AB_id, NF90_NOWRITE, ncid_read_AB)
			stat = nf90_inq_varid(ncid_read_AB, Y_name , varid_AB)


			! Time loop to get all the output model variable across the studied year
			do t=1,Nbtime

				list_read(1) = t
				stat = nf90_get_var(ncid_read_AB, varid_AB, Y_AB(t), start=list_read)
			end do
	
			! Close the two netcdf file
			stat = NF90_CLOSE(ncid_read_AB)



			! Variance calcul with different formula
			! First step : Calcule the sum 
			!-----------------

	

			!-------Formula Jansen------------
				
			! Variance calcul of the matrix BA

			do t=1, Nbtime
				var_param(t,i) = var_param(t,i)+  ( (Y_B(t)-MEAN_n(t)) /(VAR(t))**(0.5)) *  (  (Y_AB(t)-MEAN_n(t)) /(VAR(t))**(0.5) - (Y_A(t)-MEAN_n(t)) /(VAR(t))**(0.5) )
			end do

			
			! Variance calcul of the matrix    

			do t=1, Nbtime
				var_tot_param(t,i) = var_tot_param(t,i)+(((Y_A(t)-MEAN_n(t))/(VAR(t))**(0.5))-((Y_AB(t)-MEAN_n(t))/(VAR(t))**(0.5)))**2
			end do



!			!-------Formula Satelli------------
!				
!			! Variance calcul of the matrix BA
!
!			do t=1, Nbtime
!				var_param(t,i)=var_param(t,i)+Y_A(t)*(Y_BA(t)-Y_B(t))
!			end do
	
!			! Variance calcul of the matrix AB
!
!			do t=1, Nbtime
!				var_tot_param(t,i)=var_tot_param(t,i)+Y_A(t)*(Y_A(t)-Y_AB(t))
!			end do
!
!
!
			!-------Formula Sobol------------
!				
!			! Variance calcul of the matrix BA
!
!			do t=1, Nbtime
!				var_param(t,i)=var_param(t,i)+Y_A(t)*(Y_BA(t)-Y_B(t))
!			end do
	
!			! Variance calcul of the matrix AB
!
!			do t=1, Nbtime
!				var_tot_param(t,i)=var_tot_param(t,i)+Y_A(t)*(Y_A(t)-Y_AB(t))
!			end do
	

		end do


		
		! Calcul mean and standard deviation  
		!-----------------------------------


		do t=1, Nbtime
			var_tot(t) = var_tot(t)+((Y_A(t)-MEAN(t))/(VAR(t))**(0.5))**2 + ((Y_B(t)-MEAN(t))/(VAR(t))**(0.5))**2 
			mean_f(t) = mean_f(t)+((Y_A(t)-MEAN(t))/(VAR(t))**(0.5))+((Y_B(t)-MEAN(t))/(VAR(t))**(0.5))

		end do






	end do
	




	! Calcul Confident level (here 0.95)  ( A mettre comme un parametre plus tard )
	!-----------------------------------


!	Z=0.95
!	r(i,j)= nombre hasard (taille (N*n_resampling))

!	n_resampling=N/10

!	do 


	! Mean calcul

	do t=1, Nbtime
		mean_f(t)=mean_f(t)/2/N


	end do




	! Standard deviation calcul

	do t=1, Nbtime
		var_tot(t)=var_tot(t)/2/N-(mean_f(t))**2

	end do
	
	




	do t=1, Nbtime
		do i = 1, D


			var_param(t,i)=var_param(t,i)/N   ! var_tot(t)-
			var_tot_param(t,i)=var_tot_param(t,i)/N/2


		end do
	end do



	do t=1, Nbtime
		do i = 1, D

			sobol_indice(t,i)=var_param(t,i) !/var_tot(t)
			sobol_indice_tot(t,i)=var_tot_param(t,i) !/var_tot(t)			

		end do
	end do

	







	path_output=trim(Y_name)//"/"//trim(Y_prof)//"/"//trim(Y_climat)//"/" &
			//"indice_sobol_"//trim(Y_prof)//trim(Y_climat)//trim(Y_name)//str_year
	open(50, file = path_output)

	do t= 1, Nbtime


		
			WRITE(50, 105)
			WRITE(50, 105, advance='no') (  sobol_indice(t,i), i=1,D )  ! advance='no'
			
105			format( 7(e12.5,1X) ) !,1X,F3.1)	

			WRITE(50, 105, advance='no')	sum(sobol_indice(t,:))	
		
	end do
	close(50)

	path_output=trim(Y_name)//"/"//trim(Y_prof)//"/"//trim(Y_climat)//"/" &
			//"indice_sobol_total"//trim(Y_prof)//trim(Y_climat)//trim(Y_name)//str_year
	open(51, file = path_output)

	do t= 1, Nbtime


			WRITE(51, 107) (  sobol_indice_tot(t,i), i=1,D )  ! advance='no'
107			format( 7(e12.5,1X) ) !,1X,F3.1)			
		
	end do
	close(51)



	path_output=trim(Y_name)//"/"//trim(Y_prof)//"/"//trim(Y_climat)//"/" &
			//trim(Y_prof)//trim(Y_climat)//trim(Y_name)//str_year
	open(52, file = path_output)
	do t= 1, Nbtime



			VAR1(1)=MEAN_n(t) 
			VAR1(2)=VAR(t)
			VAR1(3)=VAR(t)*sobol_indice(t,1) 
			VAR1(4)=VAR(t)*sobol_indice(t,2) 
			VAR1(5)=VAR(t)*sobol_indice(t,3)
			VAR1(6)=VAR(t)*sobol_indice(t,4) 
			VAR1(7)=VAR(t)*sobol_indice(t,5) 
			VAR1(8)=VAR(t)*sobol_indice(t,6)
			VAR1(9)=VAR(t)*sobol_indice(t,7) 

 
			WRITE(52,110) ( VAR1(i), i=1,2 )
110			format( 2(e12.5,1X) )
	
	end do
	close(52)




end do




end program Sobol

