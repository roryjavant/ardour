/*
    Copyright (C) 2000 Paul Davis

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

#include <cstdlib>
#include <cmath>
#include <string>

#include <gtkmm/treeview.h>

#include "pbd/error.h"

#include "ardour/session.h"

#include "editor.h"
#include "region_view.h"
#include "selection.h"
#include "time_axis_view.h"

#include "pbd/i18n.h"

using namespace ARDOUR;
using namespace PBD;
using namespace Editing;

void
Editor::keyboard_selection_finish (bool /*add*/, Editing::EditIgnoreOption ign)
{
	if (_session) {

		framepos_t start_frame = selection->time.start().frames;
		framepos_t end;
		if ((_edit_point == EditAtPlayhead) && _session->transport_rolling()) {
			end = _session->audible_frame();
		} else {
			end = get_preferred_edit_position(ign);
		}

		//snap the selection start/end
		AudioMusic start = snap_to (start_frame);

		//if no tracks are selected and we're working from the keyboard, enable all tracks (_something_ has to be selected for any range selection)
		if ( (_edit_point == EditAtPlayhead) && selection->tracks.empty() )
			select_all_tracks();

		selection->set (start, _session->audiomusic_at_frame (end));

		//if session is playing a range, cancel that
		if (_session->get_play_range())
			_session->request_cancel_play_range();

	}
}

void
Editor::keyboard_selection_begin (Editing::EditIgnoreOption ign)
{
	if (_session) {

		framepos_t start_frame;
		AudioMusic end = selection->time.end_am();
		if ((_edit_point == EditAtPlayhead) && _session->transport_rolling()) {
			start_frame = _session->audible_frame();
		} else {
			start_frame = get_preferred_edit_position(ign);
		}

		//snap the selection start/end
		AudioMusic start = snap_to(start_frame);

		//if there's not already a sensible selection endpoint, go "forever"
		if (start > end) {
#ifdef MIXBUS
			// 4hours at most.
			// This works around a visual glitch in red-bordered selection rect.
			end.frames = start.frames + _session->nominal_frame_rate() * 60 * 60 * 4;
#else
			end.frames = max_framepos;
#endif
			end.qnotes = _session->tempo_map().quarter_note_at_frame (end.frames);
		}

		//if no tracks are selected and we're working from the keyboard, enable all tracks (_something_ has to be selected for any range selection)
		if ( selection->tracks.empty() )
			select_all_tracks();

		selection->set (start, end);

		//if session is playing a range, cancel that
		if (_session->get_play_range())
			_session->request_cancel_play_range();
	}
}

void
Editor::keyboard_paste ()
{
	paste (1, false);
}
