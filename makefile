MPPL =      ~ebelova/MPP/MPPL
 INFLAGS =   -fdefault-real-8 -o  # gnu
#INFLAGS =   -s real64 -o         # Cray
#INFLAGS =   -r8 -o                # Intel and nvidia
#INFLAGS =   -real-size 64 -o
DBG1 =       -g -init=arrays -init=zero -no-vec -WB
DBG_FLAGS =  $(DBG1) $(INFLAGS)
OPT_FLAGS =  -fast
#HYBM_FLAGS = $(OPT_FLAGS) $(INFLAGS)
HYBM_FLAGS = -fallow-argument-mismatch -std=legacy $(INFLAGS)
MPI_f90 = ftn
F90 = ftn


exam.x : exam.f
	$(MPI_f90) -o exam.x exam.f

hybm.x : hybm.p
	$(MPPL) hybm
	$(F90) -r8 -O2 -o hybm.x hybm.f

hybm_mpi.x : hybm.p
	$(MPPL) hybm
	$(MPI_f90) $(HYBM_FLAGS) hybm_mpi.x hybm.f

hybm_test.x : hybm_test.p
	$(MPPL) hybm_test
	$(F90) $(HYBM_FLAGS) hybm_test.x hybm_test.f

hybm_test_mpi.x : hybm_test.p
	$(MPPL) hybm_test
	$(MPI_f90) $(HYBM_FLAGS) hybm_test_mpi.x hybm_test.f

hmin.x : hmin.p
	$(MPPL) hmin
	$(F90) $(INFLAGS) hmin.x hmin.f

frcin.x : frcin.p
	$(MPPL) frcin
	$(F90) $(INFLAGS) frcin.x frcin.f

frcin_read.x : frcin_read.p
	$(MPPL) frcin_read
	$(F90) $(INFLAGS) frcin_read.x frcin_read.f

tkin_mpi.x : tkin.p
	$(MPPL) tkin
	$(MPI_f90) $(HYBM_FLAGS) tkin_mpi.x tkin.f

tkinm_mpi.x : tkinm.p
	$(MPPL) tkinm
	$(MPI_f90) $(HYBM_FLAGS) tkinm_mpi.x tkinm.f

tkin_read_mpi.x : tkin_read.p
	$(MPPL) tkin_read
	$(MPI_f90) $(HYBM_FLAGS) tkin_read_mpi.x tkin_read.f

hmen.x : hmen.p
	$(MPPL) hmen
	$(F90) $(INFLAGS) hmen.x hmen.f
	rm hmen.f

hist.x : hist.p
	$(MPPL) hist
	$(F90) $(INFLAGS) hist.x hist.f
	rm hist.f

en.x : en.p
	$(MPPL) en
	$(F90) $(INFLAGS) en.x en.f
	rm en.f

hfh.x : hfh.p
	$(MPPL) hfh
	$(F90) $(INFLAGS) hfh.x hfh.f
	rm hfh.f

hfhi.x : hfhi.p
	$(MPPL) hfhi
	$(F90) $(INFLAGS) hfhi.x hfhi.f
	rm hfhi.f

hptr.x : hptr.p
	$(MPPL) hptr
	$(F90) $(INFLAGS) hptr.x hptr.f
	rm hptr.f

omega.x : omega.p
	$(MPPL) omega
	$(F90) $(INFLAGS) omega.x omega.f
	rm omega.f

inv.x : inv.p
	$(MPPL) inv
	$(F90) $(INFLAGS) inv.x inv.f
	rm inv.f

hvfld.x : hvfld.p
	$(MPPL) hvfld
	compp hvfld

hsfld.x : hsfld.p
	$(MPPL) hsfld
	compp hsfld

h3bfld.x : h3bfld.p
	$(MPPL) h3bfld
	$(F90) $(INFLAGS) h3bfld.x h3bfld.f
	rm h3bfld.f

h3sfld.x : h3sfld.p
	$(MPPL) h3sfld
	$(F90) $(INFLAGS) h3sfld.x h3sfld.f
	rm h3sfld.f

h3sfld_ff.x : h3sfld_ff.p
	$(MPPL) h3sfld_ff
	$(F90) $(INFLAGS) h3sfld_ff.x h3sfld_ff.f
	rm h3sfld_ff.f

h3sfld_movie.x : h3sfld_movie.p
	$(MPPL) h3sfld_movie
	$(F90) $(INFLAGS) h3sfld_movie.x h3sfld_movie.f
	rm h3sfld_movie.f

h3sfld_movie1.x : h3sfld_movie1.p
	$(MPPL) h3sfld_movie1
	$(F90) $(INFLAGS) h3sfld_movie1.x h3sfld_movie1.f
	rm h3sfld_movie1.f

h3sfld_movie_ff.x : h3sfld_movie_ff.p
	$(MPPL) h3sfld_movie_ff
	$(F90) $(INFLAGS) h3sfld_movie_ff.x h3sfld_movie_ff.f
	rm h3sfld_movie_ff.f

h2bfld_movie.x : h2bfld_movie.p
	$(MPPL) h2bfld_movie
	$(F90) $(INFLAGS) h2bfld_movie.x h2bfld_movie.f
	rm h2bfld_movie.f

h3bfld_movie.x : h3bfld_movie.p
	$(MPPL) h3bfld_movie
	$(F90) $(INFLAGS) h3bfld_movie.x h3bfld_movie.f
	rm h3bfld_movie.f

gseqn.x : gseqn.p
	$(MPPL) gseqn
	$(F90) -g -o gseqn.x gseqn.f

movie_ncdf.x : movie_ncdf.p
	$(MPPL) movie_ncdf
	$(F90) -lnetcdf -o movie_ncdf.x movie_ncdf.f

