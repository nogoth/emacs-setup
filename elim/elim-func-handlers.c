#include "elim-func-handlers.h"
func_handler handlers[] = 
  { { "account-menu-action", _h_elim_account_menu_action } ,
    { "account-menu" , _h_elim_account_menu } ,
    { "account-options", _h_elim_account_options } ,
    { "add-account"  , _h_elim_add_account } ,
    { "add-buddy"    , _h_elim_add_buddy   } ,
    { "add-chat"     , _h_elim_add_chat    } ,
    { "buddy-menu-action", _h_elim_buddy_menu_action } ,
    { "buddy-menu"   , _h_elim_buddy_menu  } ,
    { "chat-params"  , _h_elim_chat_params } ,
    { "command"      , _h_elim_command     } ,
    { "connect"      , _h_elim_connect     } ,
    { "debug-mode"   , _h_elim_debug_mode  } ,
    { "default"      , _h_elim_default     } ,
    { "disconnect"   , _h_elim_disconnect  } ,
    { "end-conversation", _h_elim_end_conversation } ,
    { "enumerations" , _h_elim_enumerations } ,
    { "get-prefs"    , _h_elim_get_prefs   } ,
    { "init"         , _h_elim_init        } ,
    { "join-chat"    , _h_elim_join_chat   } ,
    { "list-accounts", _h_elim_list_accounts } ,
    { "list-protocols", _h_elim_list_protocols } ,
    { "message"      , _h_elim_message     } ,
    { "register"     , _h_elim_register    } ,
    { "remove-account", _h_elim_remove_account } ,
    { "remove-buddy" , _h_elim_remove_buddy } ,
    { "response"     , _h_elim_response    } ,
    { "set-account-options", _h_elim_set_account_options } ,
    { "set-prefs"    , _h_elim_set_prefs   } ,
    { "status"       , _h_elim_status      } ,
    { "unregister"   , _h_elim_unregister  } ,
    {  NULL          , NULL                } };
