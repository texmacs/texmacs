
/******************************************************************************
* MODULE     : aqua_other_widgets.h
* DESCRIPTION: some aqua widgets class declarations
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef AQUA_OTHER_WIDGETS_H
#define AQUA_OTHER_WIDGETS_H


#include "aqua_widget.h"


@class TMWidgetHelper;
@class TMButtonsController;

class aqua_tm_widget_rep: public aqua_view_widget_rep {
public:	
  NSScrollView *sv;
	NSTextField *leftField, *rightField;
	TMButtonsController *bc;
	TMWidgetHelper *wh;
	NSToolbar *toolbar;
	
  
  aqua_widget int_prompt;
  aqua_widget int_input;

  bool visibility[5]; 

public:
  aqua_tm_widget_rep (int mask = 0);
  ~aqua_tm_widget_rep ();
	
	virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
	//  virtual void notify (slot s, blackbox new_val);
	
	//  virtual void connect (slot s, widget w2, slot s2);
	//  virtual void deconnect (slot s, widget w2, slot s2);
	virtual widget plain_window_widget (string s);
	
	void layout();
  void updateVisibility();
  void do_interactive_prompt();
};


@class TMWindowController;

class aqua_window_widget_rep: public widget_rep {
public:	
	TMWindowController *wc;
	
public:
  aqua_window_widget_rep (NSWindow *win);
  ~aqua_window_widget_rep ();
	
	virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
	//  virtual void connect (slot s, widget w2, slot s2);
	//  virtual void deconnect (slot s, widget w2, slot s2);
	
	TMWindowController *get_windowcontroller();
};


#endif // defined AQUA_OTHER_WIDGETS_H
