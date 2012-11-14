#ifndef K3_H
#define K3_H

#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <tr1/unordered_map>
#include <vector>
#include <boost/enable_shared_from_this.hpp>
#include <boost/filesystem.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/program_options.hpp>
#include <boost/random/taus88.hpp>
#include <boost/random/uniform_smallint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/tokenizer.hpp>

namespace k3 {

using namespace std;
using namespace tr1;
using namespace boost;
using namespace boost::filesystem;
using namespace boost::iostreams;
using namespace boost::program_options;
using namespace boost::random;

// Types
typedef int target;
typedef pair<string, int> address;
typedef pair<int, shared_ptr<boost::any> > flow_event;

// Utility functions
string address_as_string(address& addr) {
  return addr.first + ":" + boost::lexical_cast<string>(addr.second);
}

//////////////////////
//
// Function objects

// Iterables and sockets
template<typename output> class iterable {
public:
  virtual output next() = 0;
};

template<typename input> class reactor {
public:
  virtual void react(input i) = 0;
};

// Parsers
template<typename output> class parser {
public:
  virtual output parse(string& input) = 0;

  // CSV tokenization.
  // TODO: this is inefficient, and involves significant copying, but is
  // typesafe for now in K3's imperative representation.
  // We should specialize our code generator to use string::iterator to avoid
  // copying as a vector of strings in the return value.
  vector<string> tokenize(string& input, string delim = ",",
                          int max_tokens = numeric_limits<int>::max())
  {
    typedef tokenizer< char_separator<char> > splitter;
    vector<string> r;
    char_separator<char> sep(delim);
    splitter tokens(input, sep);
    splitter::iterator it = tokens.begin();
    splitter::iterator end = tokens.end();
    for (int i = 0; it != end && i < max_tokens; ++it, ++i) { r.push_back(*it); }
    return r;
  }

  // TODO: add JSON parsing, e.g. Jansson/libjson.
};

// Dispatchers
class trigger_dispatch : public reactor<shared_ptr<boost::any> > {};

template<typename message>
class recv_dispatch : public reactor<message> {};

// Generic dispatch table for function objects.
template<typename key, typename dispatch_object>
class dispatcher {
  typedef unordered_map<key, shared_ptr<dispatch_object> > dispatch_table;
  dispatch_table dispatch;

public:
  shared_ptr<dispatch_object> dispatch_for(key& k) {
    shared_ptr<dispatch_object> r;
    typename dispatch_table::iterator it = dispatch.find(k);
    if ( it != dispatch.end() ) { r = it->second; }
    return r;
  }

  // Add to the dispatch table
  void register_dispatch (key k, shared_ptr<dispatch_object> d) {
    typename dispatch_table::iterator it = dispatch.find(k);
    if ( it == dispatch.end() ) {
      dispatch[k] = d;
    } else {
      cout << "Attempted to add duplicate key " << k << " in dispatch table." << endl;
    }
  }
};


///////////////////////////
//
// Flow program interfaces

class flow_reactor : public reactor<flow_event> {};

class flow_resources {
public:
  virtual bool is_file(int source_id) = 0;
  virtual bool is_network(int source_id) = 0;
  virtual pair<bool, shared_ptr<flow_event> > run_file(int source_id) = 0;
  virtual void run_network() = 0;
};

class flow_executor {
public:
  virtual shared_ptr<flow_reactor> get_reactor() = 0;
};


/////////////////////////
//
// I/O components.

// Framing
enum frame_type { fixed_size, delimited, variable_size };
struct frame_descriptor {
    frame_type type;
    int size;
    string delimiter;
    int off_to_size;
    int off_to_end;

    frame_descriptor() : type(delimited), size(0), delimiter("\n") {}
    frame_descriptor(string d) : type(delimited), size(0), delimiter(d) {}
    frame_descriptor(int sz) : type(fixed_size), size(sz) {}

    frame_descriptor(int os, int oe)
      : type(variable_size), size(0), off_to_size(os), off_to_end(oe)
    {}
};

// Sources, encapsulating generic framing and buffering for external data.
class source {
  int source_id;

public:
  source(int id, frame_descriptor f) : source_id(id), frame(f) {
    if ( frame.type == delimited ) {
      dd = frame.delimiter+frame.delimiter;
      dd_size = dd.size();
    }
  }

  int get_source_id() { return source_id; }

protected:
  frame_descriptor frame;
  shared_ptr<string> buffer;

  string dd;
  size_t dd_size;

  bool has_frame() {
    bool r = false;
    if ( frame.type == fixed_size ) {
      r = buffer && buffer->size() >= frame.size;
    } else if ( frame.type == delimited ) {
      r = buffer && (buffer->find(frame.delimiter) != string::npos);
    }
    return r;
  }

  shared_ptr<string> frame_from_buffer() {
    shared_ptr<string> r;
    if (frame.type == fixed_size) {
      r = shared_ptr<string>(new string(buffer->substr(0,frame.size)));
      buffer = shared_ptr<string>(new string(buffer->substr(frame.size, string::npos)));
    }
    else if ( frame.type == delimited ) {
      size_t delim_pos = buffer->find(frame.delimiter);
      r = shared_ptr<string>(new string(buffer->substr(0, delim_pos)));
      buffer = shared_ptr<string>(new string(
        buffer->substr(delim_pos+frame.delimiter.size(), string::npos)));
    }
    return r;
  }

  void append_to_buffer(char* buf, streamsize buf_size) {
    (*buffer) += string(buf, buf_size);
  }

  void append_to_buffer(string buf) { (*buffer) += buf; }

  void append_delimiter() { (*buffer) += frame.delimiter; }

  bool terminated_buffer() {
    return buffer->find(frame.delimiter) == (buffer->size()-frame.delimiter.size());
  }

  // Removes empty frames from the buffer.
  void clean_buffer() {
    size_t dd_index = 0;
    while ( (dd_index = buffer->find(dd,dd_index)) != string::npos ) {
      buffer->replace( dd_index, dd_size, frame.delimiter );
    }
  }
};


// File I/O
template<typename event>
class file_source : public iterable<event>, public source {
  path file_path;
  shared_ptr<parser<event> > file_parser;

  typedef stream<boost::iostreams::file_source> file_handle;
  shared_ptr<file_handle> file;

public:
  file_source(int id, frame_descriptor f, path fp,
              shared_ptr<parser<event> > p)
    : source(id, f), file_path(fp), file_parser(p)
  {
    file = shared_ptr<file_handle>(new file_handle(file_path.native()));
    if ( !file ) {
        cerr << "failed to open file source " << file_path << endl;
    } else { cout << "reading from " << file_path << endl; }
    buffer = shared_ptr<string>(new string());
  }

  bool has_inputs() { return has_frame() || file->good(); }

  event next() {
    event r;
    shared_ptr<string> next = next_frame();
    if ( next ) {
      r = file_parser->parse(*next);
    } else { cerr << "Could not read next frame for source " << get_source_id() << endl; }
    return r;
  }

private:

  shared_ptr<string> next_frame() {
    shared_ptr<string> r;
    size_t buf_size = (frame.size<1024) ? 1024 : frame.size;
    char buf[buf_size];

    size_t size_to_read =
      frame.type == fixed_size ?
        frame.size : (frame.type == delimited? buf_size: 0);

    if ( frame.type == fixed_size || frame.type == delimited ) {
      while ( file->good() && !has_frame() ) {
        file->read(buf, size_to_read);
        append_to_buffer(buf, file->gcount());
      }

      if ( frame.type == delimited ) {
        // Clean up on end of file by adding a delimiter.
        if( !file->good() && !terminated_buffer() ) { append_delimiter(); }
        clean_buffer();
      }
    }
    else if ( frame.type == variable_size ) {
      cerr << "variable size frames not supported" << endl;
    }
    else {
      cerr << "invalid frame type" << endl;
    }

    if ( has_frame() ) r = frame_from_buffer();
    return r;
  }
};


// Network I/O
template<typename message>
class network_handle : public reactor<message>, public source {
  bool activated;
  address addr;

public:

  network_handle(int id, frame_descriptor f, address a)
    : source(id, f), activated(false), addr(a)
  {}

  address get_address() { return addr; }

  bool is_activated() { return activated; }
  void activate() { activated = true; }
  void deactivate() { activated = false; }

  virtual void react(message event) = 0;
};


// External network sources
template<typename output>
class network_source : public network_handle<string> {
  shared_ptr<parser<output> > net_parser;
  shared_ptr<flow_executor> executor;

public:
  network_source(int id, frame_descriptor f, address addr,
                 shared_ptr<parser<output> > p)
    : network_source(id, f, addr), net_parser(p)
  {}

  void set_executor(shared_ptr<flow_executor> e) { executor = e; }

  void react(string message) {
    // Drop the event unless activated.
    if ( net_parser && is_activated() ) {
      // TODO: the buffer can grow unboundedly. Spill/drop after a threshold.
      append_to_buffer(message);
      shared_ptr<flow_reactor> reactor = executor->get_reactor();
      if ( reactor && has_frame() ) {
        shared_ptr<string> frame = frame_from_buffer();
        reactor->react(make_pair(get_source_id(), net_parser->parse(*frame)));
      } else if ( !reactor ) {
        cerr << "No current instruction for source " << get_source_id() << endl;
      }
    }
  }
};


// Specializations that require parsers to produce a top type.
class k3_file_source : public file_source< shared_ptr<boost::any> > {};
class k3_network_source : public network_source < shared_ptr<boost::any> > {};


////////////////////////////////////
//
// External protocol interfaces.
// These should be implemented by a network socket framework, and must provide
// an implementation for network handles.

class sender {
public:
  virtual void send(int target, address addr, string payload) = 0;
  virtual void open_connection(address addr) = 0;
  virtual void close_connection(address addr) = 0;
};

// Receivers provide internal messaging between triggers, and can implement
// the network layer for external sources when they are added.
class network_receiver {
public:
  virtual void init() = 0;
  virtual void run() = 0;

  virtual void add_network_source(shared_ptr<k3_network_source> h) = 0;
};

// A network receiver with dispatch capabilities.
// External implementations (e.g. ZeroMQ/Boost ASIO) should inherit from this.
template<typename key, typename recv_message>
class receiver : public network_receiver,
                 public dispatcher<key, recv_dispatch<recv_message> >
{};


//////////////////////
//
// Runtime components.

class symbol_table {
  // TODO: make this a bi-map
  typedef unordered_map<string, int> table;
  table symbols;

public:
  virtual void init() = 0;

  bool has_symbol(string& name) { return symbols.find(name) != symbols.end(); }

  pair<bool, int> get_symbol(string& name) {
    table::iterator it = symbols.find(name);
    if ( it == symbols.end() ) { return make_pair(true, it->second); }
    return make_pair(false, -1);
  }

  void add_symbol(string& name, int id) {
    table::iterator it = symbols.find(name);
    if ( it == symbols.end() ) { symbols[name] = id; }
    else { cout << "Duplicate symbol for " << name << "->" << id << endl; }
  }

  void remove_symbol(string& name) { symbols.erase(name); }
};


// Scheduler
class runtime : public dispatcher<target, trigger_dispatch> {

  // TODO: use a lock-free queue library here.
  typedef pair<target, shared_ptr<boost::any> > task;
  typedef queue<task> scheduler_queue;

  bool terminated;
  scheduler_queue queue;

public:
  runtime() : terminated(false) {}

  virtual void init() = 0;

  // Add to the scheduler queue
  void schedule (target k, shared_ptr<boost::any> data) {
    queue.push(make_task(k,data));
  }

  void run_task() {
    if ( !queue.empty() ) {
      task task = queue.front();
      target k = get_key(task);
      shared_ptr<trigger_dispatch> d = dispatch_for(k);
      if ( d ) { d->react(get_payload(task)); }
      else { cerr << "No trigger dispatch found for " << get_key(task) << endl; }
    }
  }

  void run() {
    while ( !terminated ) { run_task(); }
  }

  void terminate() { terminated = true; }

private:

  // Task manipulators.
  task make_task(target k, shared_ptr<boost::any> data) {
    return make_pair(k, data);
  }

  target get_key(task& t) { return t.first; }
  shared_ptr<boost::any> get_payload(task& t) { return t.second; }
};


////////////////////////////
//
// Flow program FSM components


// Flow instructions implement a resource pattern.
// A consume instruction will run a flow instruction, and the FSM defined by
// the pattern.
// For each pattern in a K3 role, we generate a specialized subclass of this
// flow instruction, implementing its FSM stepper.
class flow_instruction : public flow_reactor {

  shared_ptr<flow_resources> resources;
  int state;
  set<int> next_sources;

  // State id -> source ids to access next.
  typedef map<int, set<int> > state_source_table;
  state_source_table next_access;

  int get_event_source(flow_event pe) { return pe.first; }
  shared_ptr<boost::any> get_event_data(flow_event pe) { return pe.second; }

public:
  virtual void init() = 0;
  virtual bool files_done() = 0;
  virtual int run_step(int source_id, shared_ptr<boost::any> data) = 0;

  // Resource env helpers
  shared_ptr<flow_resources> get_resources() { return resources; }
  void set_resources(shared_ptr<flow_resources> r) { resources = r; }

  // FSM helpers
  void add_next_access(int state_id, set<int> source_ids) {
    state_source_table::iterator it = next_access.find(state_id);
    if ( it == next_access.end() ) { next_access[state_id] = source_ids; }
    else { cout << "Duplicate state id " << state_id << " for populating next_access." << endl; }
  }

  void next_step(int next_state) {
    state = next_state;
    state_source_table::iterator it = next_access.find(state);
    if ( it != next_access.end() ) { next_sources = it->second; }
    else { cout << "No sources found for state " << state << endl; }
  }

  // I/O helpers

  // Filters ids to those that are files.
  set<int> get_files(set<int> source_ids) {
    set<int> r;
    set<int>::iterator end = next_sources.end();
    for (set<int>::iterator it = next_sources.begin(); it != end; ++it) {
      bool iterable = resources? resources->is_file(*it) : false;
      if ( iterable ) { r.insert(*it) ;}
    }
    return r;
  }

  pair<set<int>, shared_ptr<flow_event> > next_data() {
    pair<set<int>, shared_ptr<flow_event> > r;
    set<int> file_sources = get_files(next_sources);
    set<int> failed_sources;
    while ( !(r.second || file_sources.empty()) ) {
      int source = random_element(file_sources);
      pair<bool, shared_ptr<flow_event> > p = resources->run_file(source);
      if ( p.first ) { r.second = p.second; }
      else {
        // Continue with non-failed files.
        set<int> diff;
        failed_sources.insert(source);
        set_difference(file_sources.begin(), file_sources.end(),
                       failed_sources.begin(), failed_sources.end(),
                       std::inserter(diff, diff.end()));
        file_sources = diff;
      }
    }

    r.first = failed_sources;
    return r;
  }

  void run_until_network() {
    while ( !files_done() ) {
      // TODO: what to do with failed sources?
      pair<set<int>, shared_ptr<flow_event> > r = next_data();
      if ( r.second ) {
        flow_event e = *(r.second);
        next_step(run_step(get_event_source(e), get_event_data(e)));
      } else {
        cout << "No event found on any source during pull." << endl;
      }
    }
  }

  // Run the resource environment, which has the responsibility of running
  // any I/O event loop for associated network sources.
  void run_with_network() {
    if ( resources ) { resources->run_network(); }
  }

  // Running the instruction will first run any non-network sources until
  // the mixing policy indicates we should include network sources. At that
  // point, we are in a push-based model, where each network event may lead
  // to further execution of the ready sources.
  void run() {
    if ( !files_done() ) { run_until_network(); }
    run_with_network();
  }

  // Reactor implementation
  virtual void react(flow_event e) {
    next_step(run_step(get_event_source(e), get_event_data(e)));
    run_until_network();
  }

private:
  taus88 element_rng;

  int random_element(set<int>& elements) {
    uniform_smallint<> element_idx(0, elements.size());
    int idx = element_idx(element_rng);
    set<int>::iterator it = elements.begin();
    advance(it, idx);
    return *it;
  }
};


////////////////////////////////////
//
// Top-level flow program components

// A resource environment, that tracks external file and network sources.
class resource_env : public flow_resources {

  typedef map<int, shared_ptr<k3_file_source> > source_files;
  typedef map<int, shared_ptr<k3_network_source> > source_sockets;

  shared_ptr<flow_executor> executor;
  shared_ptr<network_receiver> recvr;
  source_files files;
  source_sockets sockets;

public:
  resource_env(shared_ptr<flow_executor> e, shared_ptr<network_receiver> r)
    : executor(e), recvr(r)
  {}

  virtual void init() { if ( recvr ) { recvr->init(); } }
  virtual void run_network() { if ( recvr ) { recvr->run(); } }

  // Accessors
  shared_ptr<network_receiver> get_receiver() { return recvr; }


  // External source management
  bool is_file(int id) { return files.find(id) != files.end(); }
  bool is_network(int id) { return sockets.find(id) != sockets.end(); }

  void add_file(int id, shared_ptr<k3_file_source> i) {
    source_files::iterator it = files.find(id);
    if ( it == files.end() ) {
      files[id] = i;
    }
    else { cout << "Duplicate iterable for source " << id << endl; }
  }

  shared_ptr<k3_file_source> get_file(int id) {
    shared_ptr<k3_file_source> p;
    source_files::iterator it = files.find(id);
    if ( it != files.end() ) { p = it->second; }
    else { cout << "No valid iterable for " << id << endl; }
    return p;
  }

  // All network sources are added to the receiver for processing as well
  // as being tracked here.
  void add_network(int id, shared_ptr<k3_network_source> r) {
    source_sockets::iterator it = sockets.find(id);
    if ( it == sockets.end() ) {
      r->set_executor(executor);
      sockets[id] = r;
      recvr->add_network_source(r);
    }
    else { cout << "Duplicate reactor for source " << id << endl; }
  }

  shared_ptr<k3_network_source> get_network(int id) {
    shared_ptr<k3_network_source> p;
    source_sockets::iterator it = sockets.find(id);
    if ( it != sockets.end() ) { p = it->second; }
    else { cout << "No valid reactor for " << id << endl; }
    return p;
  }

  // Resource execution.
  pair<bool, shared_ptr<flow_event> > run_file(int id) {
    pair<bool, shared_ptr<flow_event> > r;
    source_files::iterator it = files.find(id);
    if ( it != files.end() ) {
      shared_ptr<k3_file_source> f = it->second;
      if ( f->has_inputs() ) {
        shared_ptr<flow_event> e(new flow_event(f->get_source_id(), f->next()));
        r.first = false;
        r.second = e;
      } else { r.first = true; }
    } else { cerr << "No file found for " << id << endl; }
    return r;
  }

};


// Flow roles implement K3 roles.
// This includes a set of instructions and a set of resources (file and network)
// as defined by the role.
// This class also maintains a current_instruction, which may be used externally
// to determine progress through the role's instructions.
class flow_role : public flow_executor, public enable_shared_from_this<flow_role> {

  shared_ptr<resource_env> resources;
  set<shared_ptr<flow_instruction> > instructions;
  shared_ptr<flow_instruction> current_instruction;

public:
  flow_role(shared_ptr<network_receiver> recv) {
    resources = shared_ptr<resource_env>(
                  new resource_env(shared_from_this(), recv));
  }

  virtual void init() = 0;

  shared_ptr<flow_reactor> get_reactor() { return current_instruction; }

  void add_instruction(shared_ptr<flow_instruction> i) {
    i->set_resources(resources);
    instructions.insert(i);
  }

  void run() {
    set<shared_ptr<flow_instruction> >::iterator it = instructions.begin();
    set<shared_ptr<flow_instruction> >::iterator end = instructions.end();

    for (; it != end; ++it) {
      current_instruction = *it;
      current_instruction->init();
      current_instruction->run();
    }
  }
};


///////////////////////////
//
// Program options

struct k3_options {
  shared_ptr<symbol_table> trigger_ids;
  shared_ptr<options_description> opt_desc;
  variables_map opt_map;
  positional_options_description pos_options;

  string role;

  path logdir;
  set<int> logged_triggers;

  k3_options(shared_ptr<symbol_table> trigs, int argc = 0, char* argv[] = 0) {
    trigger_ids = trigs;
    init(argc, argv);
  }

  void init(int argc, char* argv[]) {
    if (argc <= 0) return;

    opt_desc = shared_ptr<options_description>(
        new options_description("K3 program options"));

    init_options(opt_desc);
    init_positional_options(pos_options);
    process_options(argc, argv, *opt_desc, pos_options, opt_map);
    post_process_options();
  }

  void init_options(shared_ptr<options_description> desc) {
    desc->add_options()
      ("help", "list available options")
      ("logdir", value<string>(), "logging directory")
      ("logtrigs,l",
        value<vector<string> >(), "log K3 triggers")
      ("role", value<string>(&role), "K3 program role");
  }

  void init_positional_options(positional_options_description& p) {}

  void process_options(int argc, char* argv[],
                       options_description& o,
                       positional_options_description& p,
                       variables_map& m)
  {
    try {
      store(command_line_parser(argc,argv).
        options(o).positional(p).run(), m);
      notify(m);
    } catch (unknown_option& o) {
      cerr << "unknown option: \""
           << o.get_option_name() << "\"" << endl;
      cerr << *opt_desc << endl;
      exit(1);
    } catch (error& e) {
      cerr << "error parsing command line options" << endl;
      cerr << *opt_desc << endl;
      exit(1);
    }
  }

  void post_process_options() {
    // Set up logging directory.
    if ( opt_map.count("logdir") ) { logdir = opt_map["logdir"].as<string>(); }
    else { logdir = current_path(); }
    logdir = logdir.make_preferred();

    // Set up logged triggers.
    if ( opt_map.count("logtrigs") ) {
      vector<string> trigs_to_log = opt_map["logtrigs"].as<vector<string> >();
      vector<string>::iterator end = trigs_to_log.end();
      for (vector<string>::iterator it = trigs_to_log.begin(); it != end; ++it) {
        if ( trigger_ids && trigger_ids->has_symbol(*it) ) {
          logged_triggers.insert(trigger_ids->get_symbol(*it).second);
        } else { cout << "No symbol found for trigger " << *it << endl; }
      }
    }
  }

  bool help() {
    if ( opt_map.count("help") ) { cout << *opt_desc << endl; }
    return opt_map.count("help");
  }

  // Accessors
  string get_role() { return role; }

};

}

#endif
