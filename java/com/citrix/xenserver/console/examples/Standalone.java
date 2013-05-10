/*
 * Copyright (c) 2008-2011 Citrix Systems, Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
package com.citrix.xenserver.console.examples;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.apache.xmlrpc.XmlRpcException;

import com.xensource.xenapi.APIVersion;
import com.xensource.xenapi.Connection;
import com.xensource.xenapi.Console;
import com.xensource.xenapi.Session;
import com.xensource.xenapi.VM;
import com.xensource.xenapi.Types.BadServerResponse;
import com.xensource.xenapi.Types.ConsoleProtocol;
import com.xensource.xenapi.Types.XenAPIException;
import com.xensource.xenapi.VM.Record;

public class Standalone extends JFrame {
	private static final long serialVersionUID = 7324169253230998836L;

	public Standalone() {
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(serverPane(), BorderLayout.NORTH);
		getContentPane().add(vmListPane(), BorderLayout.CENTER);
		setSize(600, 200);
	}
	
	private JTextField server_url = new JTextField("http://<server>");
	private JTextField username = new JTextField();
	private JPasswordField password = new JPasswordField();
	
	private Component serverPane() {
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.LINE_AXIS));
		mainPanel.add(new JLabel("Server URL:"));
		mainPanel.add(server_url);
		mainPanel.add(new JLabel("User:"));
		mainPanel.add(username);
		mainPanel.add(new JLabel("Pass:"));
		mainPanel.add(password);
		JButton connect = new JButton("Get VM List");
		getRootPane().setDefaultButton(connect);
		connect.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				try {
					connect();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
		mainPanel.add(connect);
		
		return mainPanel;
	}
	
	Connection conn = null;
	private void connect() throws BadServerResponse, XenAPIException, XmlRpcException, MalformedURLException {
		if (conn != null)
			disconnect();
		Connection tmpConn = new Connection(new URL(server_url.getText())); 
		Session.loginWithPassword(tmpConn, username.getText(), new String(
				password.getPassword()), APIVersion.latest().toString());
		conn = tmpConn;
		populateVmList();
	}
	
	static class VMEntry
	{
		Record record = null;
		public VMEntry(Record record) {
			this.record = record;
		}
		
		@Override
		public String toString() {
			return "dom "+record.domid+": "+record.nameLabel;
		}
		
	}
	
	private void populateVmList() throws BadServerResponse, XenAPIException, XmlRpcException {
		Map<VM, Record> rec = VM.getAllRecords(conn);
		for (Map.Entry<VM, Record> e:rec.entrySet())
		{
			if (e.getValue().domid > 0)
				model.addElement(new VMEntry(e.getValue()));
		}
	}
	
	private void disconnect() throws BadServerResponse, XenAPIException, XmlRpcException {
		model.clear();
		try
		{
			Session.logout(conn);
		}
		catch (XenAPIException e)
		{
			// Ignore
		}
		conn = null;
	}

	private DefaultListModel model = new DefaultListModel();
	private Component vmListPane() {
		final JList list = new JList(model);
		JScrollPane scroll = new JScrollPane(list);
		
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent arg0) {
				if (arg0.getValueIsAdjusting())
					return;
				try {
					showVmConsole(list.getSelectedIndex());
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
		
		return scroll;
	}
	
	private void showVmConsole(int firstIndex) throws BadServerResponse, XenAPIException, XmlRpcException, Exception {
		
		// Get the console ID for the VM
		VMEntry vm = (VMEntry) model.getElementAt(firstIndex);
		System.out.println(conn.getSessionReference());
		Set<Console> consoles = vm.record.consoles;
		Iterator<Console> i = consoles.iterator();
		Console c = i.next();
		while (c.getProtocol(conn) != ConsoleProtocol.RFB)
			c = i.next();
		
		System.out.println("Session reference: "+conn.getSessionReference());
		String location = c.getLocation(conn);
		
		URL consoleURL = new URL(location);
		URL connURL = new URL(server_url.getText());
		if ("".equals(consoleURL.getHost())) {
			// If console URL is missing the host, HMN may be disabled.
			// Use http and host from the original connection in this case
			location = "http://" + connURL.getHost() + consoleURL.getFile();
			System.out.println("HMN appears disabled. Fixing location to " + location);
			consoleURL = new URL(location);
		}
		System.out.println("Setting up terminal connection to "+location+" for VM "+vm);
				
		String usessl = "false";
		String port = "80";
		if ("https".equals(consoleURL.getProtocol())) {
			usessl = "true";
			port = "443";
		}
		JavaInitialize ji = new JavaInitialize();
		ji.init(new String[] {location, conn.getSessionReference(), usessl, port});
		ji.start();
		ji.setSize(800,600);
		ji.setVisible(true);
	}

	public static void main(String[] args) {
		Standalone frame = new Standalone();
		frame.setDefaultCloseOperation(EXIT_ON_CLOSE);
		frame.setVisible(true);
	}
}
