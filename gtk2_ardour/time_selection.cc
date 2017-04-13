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

AudioMusicRange&
TimeSelection::operator[] (uint32_t which)
{
	for (std::list<AudioMusicRange>::iterator i = begin(); i != end(); ++i) {
		if ((*i).id == which) {
			return *i;
		}
	}
	fatal << string_compose (_("programming error: request for non-existent audio range (%1)!"), which) << endmsg;
	abort(); /*NOTREACHED*/
	return *(new AudioMusicRange(AudioMusic(0, 0.0), AudioMusic(0, 0.0), 0)); /* keep the compiler happy; never called */
}

bool
TimeSelection::consolidate ()
{
	bool changed = false;

  restart:
	for (std::list<AudioMusicRange>::iterator a = begin(); a != end(); ++a) {
		for (std::list<AudioMusicRange>::iterator b = begin(); b != end(); ++b) {

			if (&(*a) == &(*b)) {
				continue;
			}

			if (a->coverage (b->start.frames, b->end.frames) != Evoral::OverlapNone) {
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

AudioMusic
TimeSelection::start ()
{
	if (empty()) {
		return AudioMusic (0, 0.0);
	}

	AudioMusic first (max_framepos, 0.0);

	for (std::list<AudioMusicRange>::iterator i = begin(); i != end(); ++i) {
		if ((*i).start.frames < first.frames) {
			first = (*i).start;
		}
	}
	return first;
}

AudioMusic
TimeSelection::end_frame ()
{
	if (empty()) {
		return AudioMusic (0, 0.0);
	}

	AudioMusic last (0, 0.0);

	for (std::list<AudioMusicRange>::iterator i = begin(); i != end(); ++i) {
		if ((*i).end.frames >= last.frames) {
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

	return end_frame().frames - start().frames + 1;
}
