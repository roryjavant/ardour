/*
    Copyright (C) 2003-2004 Paul Davis

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include <algorithm>

#include "pbd/error.h"
#include "ardour/types.h"

#include "time_selection.h"

#include "pbd/i18n.h"

using namespace ARDOUR;
using namespace PBD;

MusicFrameRange&
TimeSelection::operator[] (uint32_t which)
{
	for (std::list<MusicFrameRange>::iterator i = begin(); i != end(); ++i) {
		if ((*i).id == which) {
			return *i;
		}
	}
	fatal << string_compose (_("programming error: request for non-existent audio range (%1)!"), which) << endmsg;
	abort(); /*NOTREACHED*/
	return *(new MusicFrameRange(0,0,0)); /* keep the compiler happy; never called */
}

bool
TimeSelection::consolidate ()
{
	bool changed = false;

  restart:
	for (std::list<MusicFrameRange>::iterator a = begin(); a != end(); ++a) {
		for (std::list<MusicFrameRange>::iterator b = begin(); b != end(); ++b) {

			if (&(*a) == &(*b)) {
				continue;
			}

			if (a->coverage (b->start.frame, b->end.frame) != Evoral::OverlapNone) {
				a->start = (a->start < b->start) ? a->start : b->start;//std::min (a->start, b->start);
				a->end = (b->end < a->end) ? a->end : b->end; //std::max (a->end, b->end);
				erase (b);
				changed = true;
				goto restart;
			}
		}
	}

	return changed;
}

MusicFrame
TimeSelection::start ()
{
	if (empty()) {
		return 0;
	}

	MusicFrame first = max_framepos;

	for (std::list<MusicFrameRange>::iterator i = begin(); i != end(); ++i) {
		if ((*i).start.frame < first.frame) {
			first = (*i).start;
		}
	}
	return first;
}

MusicFrame
TimeSelection::end_frame ()
{
	if (empty()) {
		return 0;
	}

	MusicFrame last = 0;

	for (std::list<MusicFrameRange>::iterator i = begin(); i != end(); ++i) {
		if ((*i).end.frame >= last.frame) {
			last = (*i).end;
		}
	}
	return last;
}

framecnt_t
TimeSelection::length()
{
	if (empty()) {
		return 0;
	}

	return end_frame().frame - start().frame + 1;
}
