/* $Id: statmain.c,v 1.1 2002/09/27 12:07:57 qfwfq Exp $ */
/*
 * $Log: statmain.c,v $
 * Revision 1.1  2002/09/27 12:07:57  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 */

/*
 * proxy main function used only with lcc-win32
 */

extern RpMain(int, char **);

main(int argc, char *argv[])
{
	return	RpMain(argc, argv);
}
