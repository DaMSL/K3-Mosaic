#ifndef ZMQ_CLOUDLESS_H
#define ZMQ_CLOUDLESS_H

#include <k3.hpp>

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>

#include <cloudless/context.hpp>
#include <cloudless/device.hpp>
#include <cloudless/edges.hpp>
#include <cloudless/message.hpp>

// Internal messages between triggers.
namespace k3 {
struct trigger_msg {
  int target;
  string payload;

  trigger_msg() {}
  trigger_msg(int t, string p) : target(t), payload(p) {}
  trigger_msg(const string& data) {
    stringstream ss(data);
    boost::archive::binary_iarchive ia(ss);
    ia >> target;
    ia >> payload;
  }

  string serialize() {
    stringstream ss;
    boost::archive::binary_oarchive oa(ss);
    oa << *this;
    return ss.str();
  }
};
}

// Non-intrusive message serialization.
namespace boost {
namespace serialization {
  template<class Archive>
  void serialize(Archive& ar, k3::trigger_msg& msg, const unsigned int version) {
    ar & msg.target;
    ar & msg.payload;
  }
}
}

// Network I/O implementation
namespace k3 {

using namespace std;
using namespace cloudless;

class zmq_sender : public sender {

  // TODO: make this a cache that expires connections not used in a while.
  typedef map<address, shared_ptr<socket> > sockets;
  sockets outgoing;
  shared_ptr<address> local_address;

public:
  zmq_sender(shared_ptr<address> addr) : local_address(addr) {}

  void send(int target, address addr, string payload) {
    shared_ptr<socket> s;
    sockets::iterator it = outgoing.find(addr);

    int retries = 5;
    while ( it == outgoing.end() && retries >= 0 ) {
      open_connection(addr);
      it = outgoing.find(addr);
      --retries;
    }

    if ( it != outgoing.end() ) { s = it->second; }
    else {
      cerr << "Could not open outgoing connection" << endl;
      return;
    }

    trigger_msg t(target, payload);
    message m(t.serialize());
    s->send(m);
  }

  void open_connection(address addr) {
    shared_ptr<socket> s(new socket(*context::instance(), socket_type::PAIR));
    s->connect(get_zmq_address(addr));
    outgoing[addr] = s;
  }

  void close_connection(address addr) {
    sockets::iterator it = outgoing.find(addr);
    if ( it != outgoing.end() ) {
      it->second->close();
      outgoing.erase(addr);
    }
  }

private:
  string get_zmq_address(address& addr) {
    string r;
    if ( local_address && (addr == *local_address) ) { r = "inproc://k3"; }
    else { r = "tcp://"+address_as_string(addr); }
    return r;
  }
};


class zmq_receiver : public cloudless::device,
                     public receiver<int, shared_ptr<trigger_msg> >
{
  typedef recv_dispatch<shared_ptr<trigger_msg> > dispatch_object;

public:
  zmq_receiver(const edges& edges) : cloudless::device(edges) {}

  void on_init() {
    init();
    initialize_trigger_socket();
    cout << "ZMQ receiver started." << endl;
  }

  void on_recv(const string& edge_id, const string& edgepoint_id) {
    edgepoint::point& point = get_edges()[edge_id][edgepoint_id];

    cloudless::message msg;
    if (point.psocket->recv(msg)) {
      cout << "Received a message" << endl;

      shared_ptr<trigger_msg> t(new trigger_msg(*(msg.head())));
      cout << "Target: " << t->target << endl;

      shared_ptr<dispatch_object> d = dispatch_for(t->target);
      cout << "Dispatching: " << t->target << endl;
      d->react(t);
      cout << "Done: " << t->target << endl;
    }
  }

  void on_shutdown() {
    edges::iterator it = get_edges().begin();
    edges::iterator end = get_edges().end();
    for (; it != end; ++it) {
      edgepoint::iterator p_it = it->second.begin();
      edgepoint::iterator p_end = it->second.end();
      for (; p_it != p_end; ++p_it) {
        cout << "ZMQ receiver closing " << it->first << "::" << p_it->first << endl;
        p_it->second.psocket->close();
      }
    }
  }

  void add_network_source(shared_ptr<k3_network_source> h) {
    address h_addr = h->get_address();
    string addr = address_as_string(h_addr);
    string source_idx = "source:"+addr;
    get_edges()["k3"][source_idx].psocket->bind("tcp://"+addr);
  }

protected:

  void initialize_trigger_socket() {
    edgepoint ep;
    edgepoint::point trigger_socket = { socket_type::PAIR };
    ep.add("triggers", trigger_socket);
    get_edges().add("k3", ep);
    get_edges()["k3"]["triggers"].psocket->bind("inproc://k3");
  }
};

}

#endif
