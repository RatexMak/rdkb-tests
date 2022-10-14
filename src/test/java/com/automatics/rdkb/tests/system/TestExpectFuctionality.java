package com.automatics.rdkb.tests.system;

import java.util.List;

import org.apache.http.conn.params.ConnConnectionParamBean;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.providers.connection.DeviceConnectionProvider;
import com.automatics.providers.connection.SshConnection;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;

public class TestExpectFuctionality extends AutomaticsTestBase {

	private static DeviceConnectionProvider deviceConnectionProvider;

	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-EXPECT-TEST-1")
	public void testVerifyExpectFeature(Dut device) {

		String testCaseId = "";
		String stepNum = "1";
		String errorMessage = "";
		boolean status = false;
		String ssidName = null;
		String passPhraseName = null;
		String response = null;
		Dut deviceConnectedWith2Ghz = null;
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
							LOGGER.info("user password :" + device.getExtraProperties().get("password"));

							try {
								String connectToLinux = "nmcli d wifi connect <ssid> password <password>";
								String password = "rdk@1234";
								String[] commands = {BroadBandCommandConstants.CMD_SUDO + connectToLinux
										.replaceAll("<ssid>", ssidName).replaceAll("<password>", passPhraseName),"rdk@1234"};
								String command = BroadBandCommandConstants.CMD_SUDO + connectToLinux
										.replaceAll("<ssid>", ssidName).replaceAll("<password>", passPhraseName);
								response = execute(clientSettop, commands, command, password);
								LOGGER.info("response :"+response);
							} catch (Exception e) {
								LOGGER.info("failed to execute commands");
							}
						}else {
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
		conn.sendAsRoot(command);
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

}
