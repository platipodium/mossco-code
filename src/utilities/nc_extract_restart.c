/**

  @brief Implementation of a netcdf utility that extracts a subset of variables

  This computer program is part of MOSSCO.
  @copyright Copyright (C) 2015, Helmholtz-Zentrum Geesthacht
  @author Carsten Lemmen

  MOSSCO is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License v3+.  MOSSCO is distributed in the
  hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
  LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

  This software is modified from copyrighted software 'nccopy3.c'
  @copyright Copyrighte(C) 1993-2014, University Corporation for Atmospheric Research/Unidata

**/

#include <stdlib.h>
#include <stdio.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include <string.h>
#include <netcdf.h>

#define SAME_AS_INPUT (-1)	/* default, if kind not specified */

#define CHECK(stat,f) if(stat != NC_NOERR) {check(stat,#f,__FILE__,__LINE__);} else {}

/* These are in unistd.h; for use with getopt() */
extern int optind;
extern int opterr;
extern char *optarg;

static char *progname; 		/* for error messages */

static void check(int err, const char* fcn, const char* file, const int line) {
  fprintf(stderr,"%s\n",nc_strerror(err));
  fprintf(stderr,"Location: function %s; file %s; line %d\n",fcn,file,line);
  fflush(stderr); fflush(stdout);
  exit(1);
}

/* Check error return from malloc, and allow malloc(0) with subsequent free */
static void * emalloc (size_t size) {
  void   *p;

  p = (void *) malloc (size==0 ? 1 : size); /* don't malloc(0) */
  if (p == 0) {
	  fprintf(stderr,"Out of memory\n");
  }
  return p;
}

/* Forward declaration, because copy_type, copy_vlen_type call each other */
static int copy_type(int igrp, nc_type typeid, int ogrp);

/* get group id in output corresponding to group igrp in input,
 * given parent group id (or root group id) parid in output. */
static int get_grpid(int igrp, int parid, int *ogrpp) {
  int stat = NC_NOERR;
  int ogid;			/* like igrp but in output file */
  ogid = parid;
  *ogrpp = ogid;
  return stat;
}

/* Copy dimensions from group igrp to group ogrp */
static int copy_dims(int igrp, int ogrp) {
  int stat = NC_NOERR;
  int ndims;
  int nunlims;
  int dgrp;
  int unlimid;

  stat = nc_inq_ndims(igrp, &ndims);
  CHECK(stat, nc_inq_ndims);
  stat = nc_inq_unlimdim(igrp, &unlimid);
  CHECK(stat, nc_inq_unlimdim);

  /* Copy each dimension to output, including unlimited dimension(s) */
  for (dgrp = 0; dgrp < ndims; dgrp++) {
	  char name[NC_MAX_NAME];
	  size_t length;
	  int is_unlim;
	  int uld;
	  int dimid;

	  is_unlim = 0;
	  dimid = dgrp;
	  if(unlimid != -1 && (dimid == unlimid)) {
	    is_unlim = 1;
	  }

	  stat = nc_inq_dim(igrp, dimid, name, &length);
	  if (stat == NC_EDIMSIZE && sizeof(size_t) < 8) {
	    fprintf(stderr, "dimension \"%s\" requires 64-bit platform\n", name);
	  }
	  CHECK(stat, nc_inq_dim);
	  if(is_unlim) {
	    stat = nc_def_dim(ogrp, name, NC_UNLIMITED, NULL);
	  } else {
	    stat = nc_def_dim(ogrp, name, length, NULL);
	  }
	  CHECK(stat, nc_def_dim);
  }
  return stat;
}

/* Copy the attributes for variable ivar in group igrp to variable
 * ovar in group ogrp.  Global (group) attributes are specified by
 * using the varid NC_GLOBAL */
static int copy_atts(int igrp, int ivar, int ogrp, int ovar) {

  int natts, iatt;
  int stat = NC_NOERR;

  stat = nc_inq_varnatts(igrp, ivar, &natts);
  CHECK(stat, nc_inq_varnatts);

  for(iatt = 0; iatt < natts; iatt++) {
	  char name[NC_MAX_NAME];
	  stat = nc_inq_attname(igrp, ivar, iatt, name);
	  CHECK(stat, nc_inq_attname);
	  stat = nc_copy_att(igrp, ivar, name, ogrp, ovar);
	  CHECK(stat, nc_copy_att);
  }
  return stat;
}

/* copy the schema for a single variable in group igrp (from a file of
 * kind inkind) to group ogrp */
static int copy_var(int igrp, int inkind, int varid, int ogrp) {
    int stat = NC_NOERR;
    int ndims;
    int idimids[NC_MAX_DIMS];	/* ids of dims for input variable */
    int odimids[NC_MAX_DIMS];	/* ids of dims for output variable */
    char name[NC_MAX_NAME];
    nc_type typeid, o_typeid;
    int natts;
    int i;
    int o_varid;

    stat = nc_inq_varndims(igrp, varid, &ndims);
    CHECK(stat, nc_inq_varndims);
    stat = nc_inq_var(igrp, varid, name, &typeid, &ndims, idimids, &natts);
    CHECK(stat, nc_inq_var);
    o_typeid = typeid;

    /* get the corresponding dimids in the output file */
    for(i = 0; i < ndims; i++) {
	char dimname[NC_MAX_NAME];
	stat = nc_inq_dimname(igrp, idimids[i], dimname);
	CHECK(stat, nc_inq_dimname);
	stat = nc_inq_dimid(ogrp, dimname, &odimids[i]);
	CHECK(stat, nc_inq_dimid);
    }

    /* define the output variable */
    stat = nc_def_var(ogrp, name, o_typeid, ndims, odimids, &o_varid);
    CHECK(stat, nc_def_var);

    /* attach the variable attributes to the output variable */
    stat = copy_atts(igrp, varid, ogrp, o_varid);
    CHECK(stat, copy_atts);
    return stat;
}

/* copy the schema for all the variables in group igrp (from a file of kind
 * inkind) to group ogrp */
static int extract_vars(char* pattern, int igrp, int inkind, int ogrp) {

    int stat = NC_NOERR;
    int nvars;
    int varid;

    char name[NC_MAX_NAME+1];

    stat = nc_inq_nvars(igrp, &nvars);
    CHECK(stat, nc_inq_nvars);

    for (varid = 0; varid < nvars; varid++) {

      stat = nc_inq_varname(igrp, varid, name);
	    CHECK(stat, nc_inq_varname);

      if (strstr(name,pattern)) {
        fprintf(stdout,"%s matches pattern %s\n", name, pattern);
	      stat = copy_var(igrp, inkind, varid, ogrp);
	      CHECK(stat, copy_var);
      } else {
        //fprintf(stdout,"%s != %s\n", name, pattern);
      }
    }
    return stat;
}

/* Copy the schema in a group and all its subgroups, recursively, from
 * group igrp in input to parent group ogrp in destination.  inkind
 * is the kind of netCDF file that igrp identifies (1 -> classic, 2 -<
 * 64-bit offset, 3-> netCDF-4, 4-> netCDF-4 classic model). */
static int extract_schema(char* pattern, int igrp, int inkind, int ogrp) {
    int stat = NC_NOERR;
    int ogid;			/* like igrp but in output file */
    int i;

    /* get groupid in output corresponding to group igrp in input,
     * given parent group (or root group) ogrp in output */
    stat = get_grpid(igrp, ogrp, &ogid);
    CHECK(stat, get_grpid);

    stat = copy_dims(igrp, ogid);
    CHECK(stat, copy_dims);
    stat = copy_atts(igrp, NC_GLOBAL, ogid, NC_GLOBAL);
    CHECK(stat, copy_atts);
    stat = extract_vars(pattern, igrp, inkind, ogid);
    CHECK(stat, extract_vars);
    return stat;
}

/* Return number of values for a variable varid in a group igrp, as
 * well as start and count arrays for array access, assumed to be
 * preallocated to hold one value for each dimension of variable */
static int inq_nvals(int igrp, int varid,
	  size_t *startp, size_t *countp, size_t *nvalsp) {

  int stat = NC_NOERR;
  int ndims;
  int dimids[NC_MAX_DIMS];
  int dim;
  size_t nvals = 1;

  stat = nc_inq_varndims(igrp, varid, &ndims);
    CHECK(stat, nc_inq_varndims);
    stat = nc_inq_vardimid (igrp, varid, dimids);
    CHECK(stat, nc_inq_vardimid);
    for(dim = 0; dim < ndims; dim++) {
	size_t len;
	stat = nc_inq_dimlen(igrp, dimids[dim], &len);
	CHECK(stat, nc_inq_dimlen);
	nvals *= len;
	startp[dim] = 0;
	countp[dim] = len;
    }
    *nvalsp = nvals;
    return stat;
}

/* From netCDF type in group igrp, get size in memory needed for each
 * value */
static int
inq_value_size(int igrp, nc_type vartype, size_t *value_sizep) {
    int stat = NC_NOERR;

    switch(vartype) {
    case NC_BYTE:
	*value_sizep = sizeof(signed char);
	break;
    case NC_CHAR:
	*value_sizep = sizeof(char);
	break;
    case NC_SHORT:
	*value_sizep = sizeof(short);
	break;
    case NC_INT:
	*value_sizep = sizeof(int);
	break;
    case NC_FLOAT:
	*value_sizep = sizeof(float);
	break;
    case NC_DOUBLE:
	*value_sizep = sizeof(double);
	break;
    default:
	stat = NC_EBADTYPE;
	CHECK(stat, inq_value_size);
	break;
    }
    return stat;
}

/* Copy data from variable varid in group igrp to corresponding group
 * ogrp */
static int copy_var_data(int igrp, int inkind, int varid, int ogrp) {
    int stat = NC_NOERR;
    nc_type vartype;
    size_t value_size;		/* size in bytes of one value of variable */
    size_t nvalues;		/* number of values for this variable */
    void *buf;			/* buffer for the variable values */
    char varname[NC_MAX_NAME];
    int ovarid;
    size_t start[NC_MAX_DIMS];
    size_t count[NC_MAX_DIMS];

    /* Note: for simplicity, this example code copies a whole variable
     * at a time, so all the data's variable must fit in memory.  This
     * won't work for variables too large to fit in memory.  For an
     * approach that works efficiently in the general case for any
     * size variable, see the use of nc_get_iter() and nc_next_iter()
     * in the nccopy utlity that comes with the netCDF
     * distribution.  */
    stat = inq_nvals(igrp, varid, start, count, &nvalues);
    CHECK(stat, inq_nvals);
    if (nvalues == 0) return stat;
    stat = nc_inq_vartype(igrp, varid, &vartype);
    CHECK(stat, nc_inq_vartype);
    /* from type, get size in memory needed for each value */
    stat = inq_value_size(igrp, vartype, &value_size);
    CHECK(stat, inq_value_size);
    /* get corresponding output variable */
    stat = nc_inq_varname(igrp, varid, varname);
    CHECK(stat, nc_inq_varname);
    stat = nc_inq_varid(ogrp, varname, &ovarid);

    if (stat != NC_NOERR) return NC_NOERR;
    //CHECK(stat, nc_inq_varid);

    buf = emalloc(value_size * nvalues);
    switch(vartype) {
    case NC_BYTE:
 	    stat = nc_get_vara_schar(igrp, varid, start, count, buf);
	    CHECK(stat, nc_get_vara_schar);
	    stat = nc_put_vara_schar(ogrp, ovarid, start, count, buf);
	    CHECK(stat, nc_put_vara_schar);
	    break;
    case NC_CHAR:
	    stat = nc_get_vara_text(igrp, varid, start, count, buf);
	    CHECK(stat, nc_get_vara_text);
	    stat = nc_put_vara_text(ogrp, ovarid, start, count, buf);
	    CHECK(stat, nc_put_vara_text);
	    break;
    case NC_SHORT:
	stat = nc_get_vara_short(igrp, varid, start, count, buf);
	CHECK(stat, nc_get_vara_short);
	stat = nc_put_vara_short(ogrp, ovarid, start, count, buf);
	CHECK(stat, nc_put_vara_short);
	break;
    case NC_INT:
	stat = nc_get_vara_int(igrp, varid, start, count, buf);
	CHECK(stat, nc_get_vara_int);
	stat = nc_put_vara_int(ogrp, ovarid, start, count, buf);
	CHECK(stat, nc_put_vara_int);
	break;
    case NC_FLOAT:
	stat = nc_get_vara_float(igrp, varid, start, count, buf);
	CHECK(stat, nc_get_vara_float);
	stat = nc_put_vara_float(ogrp, ovarid, start, count, buf);
	CHECK(stat, nc_put_vara_float);
	break;
    case NC_DOUBLE:
	stat = nc_get_vara_double(igrp, varid, start, count, buf);
	CHECK(stat, nc_get_vara_double);
	stat = nc_put_vara_double(ogrp, ovarid, start, count, buf);
	CHECK(stat, nc_put_vara_double);
	break;
    default:
	CHECK(NC_EBADTYPE, copy_var_data);
	break;
    }
    free(buf);
    return stat;
}

/* Copy data from variables in group igrp to variables in
 * corresponding group with parent ogrp, and all subgroups
 * recursively  */
static int extract_data(int igrp, int inkind, int ogrp) {
    int stat = NC_NOERR;
    int ogid;
    int numgrps;
    int *grpids;
    int i;
    int nvars;
    int varid;

    /* get groupid in output corresponding to group igrp in input,
     * given parent group (or root group) ogrp in output */
    stat = get_grpid(igrp, ogrp, &ogid);
    CHECK(stat, get_grpid);

    /* Copy data from this group */
    stat = nc_inq_nvars(igrp, &nvars);
    CHECK(stat, nc_inq_nvars);
    for (varid = 0; varid < nvars; varid++) {
	stat = copy_var_data(igrp, inkind, varid, ogid);
	CHECK(stat, copy_var_data);
    }
    return stat;
}

/* copy infile to outfile using netCDF API, kind specifies which
 * netCDF format for output: 0 -> same as input, 1 -> classic, 2 ->
 * 64-bit offset, 3 -> netCDF-4, 4 -> netCDF-4 classic model */
static int extract(char* pattern, char* infile, char* outfile, int kind) {

    int stat = NC_NOERR;
    int igrp, ogrp;
    int inkind, outkind;

    stat = nc_open(infile,NC_NOWRITE,&igrp);
    CHECK(stat,nc_open);

    stat = nc_inq_format(igrp, &inkind);
    CHECK(stat,nc_inq_format);

    outkind = (kind == SAME_AS_INPUT) ? inkind : kind;

    switch(outkind) {
      case NC_FORMAT_CLASSIC:
	      stat = nc_create(outfile,NC_CLOBBER,&ogrp);
	    break;
      case NC_FORMAT_64BIT:
	      stat = nc_create(outfile,NC_CLOBBER|NC_64BIT_OFFSET,&ogrp);
	    break;
      case NC_FORMAT_NETCDF4:
      case NC_FORMAT_NETCDF4_CLASSIC:
	      fprintf(stderr,"%s built without ability to create netCDF-4 files\n",progname);
	      exit(1);
      default:
	      fprintf(stderr,"%s: bad value (%d) for -k option\n", progname, kind);
	      exit(1);
    }
    CHECK(stat,nc_create);

    stat = extract_schema(pattern, igrp, inkind, ogrp);
    CHECK(stat,extract_schema);
    stat = nc_enddef(ogrp);
    CHECK(stat, nc_enddef);
    stat = extract_data(igrp, inkind, ogrp);
    CHECK(stat,extract_data);

    stat = nc_close(igrp);
    CHECK(stat,nc_close);
    stat = nc_close(ogrp);
    CHECK(stat,nc_close);
    return stat;
}

static void
usage(void)
{
#define USAGE   "\
  [-k n]    kind of netCDF format for output file, default same as input\n\
	    1 classic, 2 64-bit offset\n\
  infile    name of netCDF input file\n\
  outfile   name for netCDF output file\n"

    (void) fprintf(stderr,
		   "%s [-k n] infile outfile\n%s",
		   progname,
		   USAGE);
}

int main(int argc, char**argv) {

  char* inputfile = NULL;
  char* outputfile = NULL;
  char* pattern = NULL;
  int kind = SAME_AS_INPUT; /* default, output same format as input */
  int c;

  opterr = 1;
  progname = argv[0];

  if (argc <= 1) {
    usage();
    return 1;
  }

  while ((c = getopt(argc, argv, "k:")) != EOF) {

	  switch(c) {
	    case 'k':
	      kind = strtol(optarg, NULL, 10);
	      break;
	    default:
	      usage();
	      exit(1);
	      break;
    }
  }

  argc -= optind;
  argv += optind;

  if (argc != 3) {
	  fprintf(stderr,"one pattern, one input file and one output file required\n");
	  exit(1);
  }

  pattern = argv[0];
  inputfile = argv[1];
  outputfile = argv[2];

  if(strcmp(inputfile, outputfile) == 0) {
	  fprintf(stderr,"output would overwrite input\n");
	  exit(1);
  }

  if (extract(pattern,inputfile, outputfile, kind) != NC_NOERR) exit(1);
  return 0;
}
