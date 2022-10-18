/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.system;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.providers.connection.SshConnection;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;

public class TestExpectFuctionality extends AutomaticsTestBase {

	private JSch jschSSHChannel;
	private Session sessionConnection;

	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-EXPECT-TEST-1")
	public void testVerifyExpectFeature(Dut device) {

		String testCaseId = "";
		String stepNum = "1";
		String errorMessage = "";
		boolean status = false;
		String ssidName = null;
		String passPhraseName = null;
		// variable to store wifi capability
		String wifiCapability = null;
		// variable to store connection type of the connected client
		String connectionType = null;
		String macAddress = null;

		// Variable Declaration Ends

		testCaseId = "TC-RDKB-EXPECT-TEST-1";

		LOGGER.info("###################################################################################");
		LOGGER.info("STARTING TEST CASE : TC-RDKB-EXPECT-TEST-1");

		try {

			List<Dut> connectedClientDevices = ((Device) device).getConnectedDeviceList();

			if (connectedClientDevices != null && connectedClientDevices.size() > 0) {
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				LOGGER.info("sssid name :" + ssidName);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				LOGGER.info("passPhraseName name :" + passPhraseName);
				for (Dut clientSettop : connectedClientDevices) {
					macAddress = clientSettop.getHostMacAddress();
					LOGGER.info("Client device Mac address : " + macAddress);

					connectionType = ((Device) clientSettop).getConnectedDeviceInfo().getConnectionType();
					LOGGER.info("Client device connection type : " + connectionType);

					wifiCapability = ((Device) clientSettop).getConnectedDeviceInfo().getWifiCapability();
					LOGGER.info("Client device Wi-Fi capability : " + wifiCapability);

					try {
						Device ecastSettop = (Device) clientSettop;
						if (ecastSettop.isLinux()) {
							LOGGER.info("user password :" + clientSettop.getExtraProperties().get("password"));

							try {
								String connectToLinux = "nmcli d wifi connect <ssid> password <password>";
								String password = "rdk@1234";
								String command = BroadBandCommandConstants.CMD_SUDO + connectToLinux
										.replaceAll("<ssid>", ssidName).replaceAll("<password>", passPhraseName);

//								response = execute(clientSettop, commands, command, password);
								String errormssage = connect(ecastSettop);
								if (errormssage != null) {
									LOGGER.info("connect failed :" + errormssage);
								}
								String responseCommand = sendCommand(ecastSettop, command, password);
								LOGGER.info("response of command 1 :" + responseCommand);
								close();

							} catch (Exception e) {
								LOGGER.info("failed to execute commands");

							}
						} else {
							LOGGER.error("no linux device");
						}
					} catch (Exception e) {
						LOGGER.info("ssh connection failed :" + e.getMessage());
					}

				}

			} else {
				LOGGER.info("no CC devices connected");
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			LOGGER.error("exception occured :" + e.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		}
		LOGGER.info("ending testcase");
	}

	/**
	 * Execute commands in device
	 * 
	 * @param dut
	 * @param command
	 * @param expectStr
	 * @param options
	 * @return response string
	 */
	public String execute(Dut dut, String[] commands, String command, String password) {
		String response = "";
		LOGGER.info("execute method invoked with device,commands: " + commands);
		SshConnection conn = new SshConnection(dut.getHostIpAddress());

		try {
//	    response = conn.send(command, expectStr, options);
//		conn.sendAsRoot(command);
			conn.connect();
			conn.sendCommand(command, 1000);
			conn.bufferResponse();
			conn.sendCommand(password, 1000);

//		conn.sendToShell(commands);
			conn.bufferResponse();

		} catch (Exception ex) {
			LOGGER.error("Exception occured while sending request DUT " + ex);
		} finally {
			if (null != conn) {
				conn.disconnect();
			}
		}

		LOGGER.info("Received response: " + response);

		return response;
	}

	public String sendCommand(Dut client, String command1, String command2) {
		StringBuilder outputBuffer = new StringBuilder();

		try {
			Channel channel = sessionConnection.openChannel("exec");
			((ChannelExec) channel).setCommand(command1);
			InputStream commandOutput = channel.getInputStream();
			((ChannelExec) channel).setCommand(command2);
			InputStream commandOutput2 = channel.getInputStream();
			channel.connect();
			int readByte = commandOutput.read();
			int readByte2 = commandOutput2.read();

			while (readByte != 0xffffffff) {
				outputBuffer.append((char) readByte);
				readByte = commandOutput.read();
			}
			while (readByte2 != 0xffffffff) {
				outputBuffer.append((char) readByte2);
				readByte2 = commandOutput.read();
			}

			channel.disconnect();
		} catch (IOException e) {
			LOGGER.info("IO execption occured : " + e);
			return null;
		} catch (JSchException e) {
			LOGGER.info("JSCH execption occured : " + e);
			return null;
		}
		return outputBuffer.toString();
	}

	public String connect(Dut client) {
		String errorMessage = null;

		try {
			String host = client.getHostIpAddress();
			LOGGER.info("host :" + host);
			String userName = "teuser";
			LOGGER.info("username :" + userName);
			String password = "tel1234#";
			int port = 22;
			sessionConnection = jschSSHChannel.getSession(userName, host, port);
			sessionConnection.setPassword(password);
			sessionConnection.connect();
		} catch (JSchException e) {
			errorMessage = e.getMessage();
		}

		return errorMessage;
	}

	public void close() {
		LOGGER.info("disconnectiing ssh connection :");
		sessionConnection.disconnect();
	}

}
