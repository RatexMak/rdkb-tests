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
package com.automatics.rdkb.tests.wifi;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandStaticIPTests extends AutomaticsTestBase {
	/**
	 * Configure Static IPv4 through CLI for business device.
	 * <ol>
	 * <li>Set the default/initial value of static IPv4 address as null using
	 * SNMP.</li>
	 * <li>Verify the default/initial value of static IPv4 address using webpa</li>
	 * <li>copy the cliconfig.txt script from resources folder to /var in the
	 * gateway device</li>
	 * <li>Get the static wan ip set for the device from the script</li>
	 * <li>Apply the configuration for static IP setting</li>
	 * <li>Wait for a 3 minutes for statics to start routing. and Verify the value
	 * of static IPv4 address using SNMP</li>
	 * <li>Verify the current value of static IPv4 address using webpa</li>
	 * <li>Connect a wifi capable client to 2.4ghz and 5ghz client</li>
	 * <li>Ping the static IP of router from connected client</li>
	 * <li>Check the Internet connectivity of connected client</li>
	 * <li>Factory reset the device using Webpa and wait till the device comes
	 * online</li>
	 * <li>Reactivate the gateway device after factory reset</li>
	 * <li>Verify the current value of static IPv4 address using SNMP</li>
	 * <li>Verify the current value of static IPv4 address using webpa</li>
	 * </ol>
	 * 
	 * @author anandam.s
	 * @refactor Govardhan
	 * @param device
	 * 
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-STATIC-IP-1000")
	public void verifyNetConnectivityWhenRouterHasStaticIP(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-STATIC-IP-100";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-STATIC-IP-1000");
		LOGGER.info("TEST DESCRIPTION: Configure Static IPv4 through CLI for business device.");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Set the  default/initial  value  of static IPv4 address  as null  using SNMP.");
		LOGGER.info("2. Verify the default/initial  value of static IPv4 address using webpa ");
		LOGGER.info("3. copy the cliconfig.txt script from resources folder to /var in the gateway device ");
		LOGGER.info("4. Get the  static wan ip set for the device from the script ");
		LOGGER.info("5. Apply the configuration for static IP setting ");
		LOGGER.info(
				"6. Wait for a 3 minutes  for statics to start routing. and Verify the  value of static IPv4 address using SNMP ");
		LOGGER.info("7. Verify the current  value of static IPv4 address using webpa ");
		LOGGER.info("8. Connect a wifi capable client to 2.4ghz and 5ghz client ");
		LOGGER.info("9. Ping the static IP of router  from connected client ");
		LOGGER.info("10. Check the Internet connectivity of  connected client ");
		LOGGER.info("11. Factory reset the device using Webpa and wait till the device comes online");
		LOGGER.info("12. Reactivate the gateway device after factory reset ");
		LOGGER.info("13. Verify the current  value of static IPv4 address using SNMP ");
		LOGGER.info("14. Verify the current  value of static IPv4 address using webpa ");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			errorMessage = "The initial /default value of IPv4 address obtained using snmp is not null.";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Set the  default/initial  value  of static IPv4 address  as null  using SNMP. ");
			LOGGER.info("STEP 1: ACTION : Execute snmpwalk using mib .1.3.6.1.4.1.17270.50.2.1.4.6.0");
			LOGGER.info(
					"STEP 1: EXPECTED : The initial  value should be null  SNMPv2-SMI::enterprises.17270.50.2.1.4.6.0 = \"\"");
			LOGGER.info("**********************************************************************************");

			// Execute SNMP get operation for the MIB dkbRgDeviceConfigStaticIp
			// (.1.3.6.1.4.1.17270.50.2.1.4.6.0)
			String response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getOid(),
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getTableIndex());
			LOGGER.info("SNMP command output for dkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0) : "
					+ response);

			if (CommonMethods.isNull(response)) {
				status = true;
			} else {
				// set the value as null
				response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getOid(), SnmpDataType.STRING, "",
						BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getTableIndex());
				tapEnv.waitTill(RDKBTestConstants.THIRTY_SECOND_IN_MILLIS);
				response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getOid(),
						BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getTableIndex());
				LOGGER.info("SNMP command output for dkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0) : "
						+ response);
				status = CommonMethods.isNull(response);
			}
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : The static IP obtained using SNMP at the initial state is null as expected ");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "The initial /default value of IPv4 address obtained using webpa is not null.";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify the default/initial  value of static IPv4 address using webpa ");
			LOGGER.info(
					"STEP 2: ACTION : Execute webpa get on dmcli eRT getv Device.X_CISCO_COM_TrueStaticIP.IPAddress");
			LOGGER.info("STEP 2: EXPECTED : The initial  value should be null "
					+ "\ndmcli eRT getv Device.X_CISCO_COM_TrueStaticIP.IPAddress"
					+ "    \nCR component name is: eRT.com.cisco.spvtg.ccsp.CR" + "    \nsubsystem_prefix eRT."
					+ "    \ngetv from/to component(eRT.com.cisco.spvtg.ccsp.pam): Device.X_CISCO_COM_TrueStaticIP.IPAddress"
					+ "    \nExecution succeed."
					+ "    \nParameter    1 name: Device.X_CISCO_COM_TrueStaticIP.IPAddress"
					+ "                  \n type:     string,    value:");
			LOGGER.info("**********************************************************************************");

			// Execute the command dmcli eRT getv Device.X_CISCO_COM_TrueStaticIP.IPAddress
			// by SSHing to the device and
			// get the static WAN IP address

			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_STATIC_WAN_IP);
			status = CommonMethods.isNull(response);

			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : The static IP obtained using webpa at the initial state is null as expected");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to copy the script for the test into the device ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : copy the cliconfig.txt script from resources folder to /var in the gateway device ");
			LOGGER.info("STEP 3: ACTION : Paste the script which is the minimum settings to get statics working.");
			LOGGER.info("STEP 3: EXPECTED : the script should be copied successfully.");
			LOGGER.info("**********************************************************************************");
			if (CommonUtils.downloadFileUsingAutoVault(device, tapEnv,
					BroadbandPropertyFileHandler.getCliConfigFileLocation(), BroadBandTestConstants.VAR_PATH)) {
				status = CommonMethods.isFileExists(device, tapEnv,
						BroadBandTestConstants.VAR_PATH.concat(BroadBandTestConstants.SCRIPT_CLICONFIG));
			}
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Successfully copied cliconfig.txt script from resources folder to /var in the gateway device ");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Failed to get the static IP set for gateway from the device ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Get the  static wan ip set for the device from the script ");
			LOGGER.info("STEP 4: ACTION : Execute  command cat cliconfig.txt | grep \"wan_ip_address\"");
			LOGGER.info("STEP 4: EXPECTED : Static gateway IP configured in the script should be obtained");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.COMMAND_GET_STATIC_IP_ADDRESS);

			String wan_ip_address = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TO_GET_WAN_IP);
			status = CommonMethods.isNotNull(wan_ip_address) && CommonMethods.isIpv4Address(wan_ip_address);
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Obtained the static wan ip from script as " + wan_ip_address);
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "Failed to apply the configuration for static IP setting ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Apply the configuration for static IP setting ");
			LOGGER.info(
					"STEP 5: ACTION : Enter command: ccsp_bus_client_tool eRT setv Device.X_CISCO_COM_TrueStaticIP.ConfigApply bool true true");
			LOGGER.info("STEP 5: EXPECTED : Webpa should be executed successfully"
					+ "\nCR component name is: eRT.com.cisco.spvtg.ccsp.CR" + "\nsubsystem_prefix eRT."
					+ "\nsetv from/to component(eRT.com.cisco.spvtg.ccsp.pam): Device.X_CISCO_COM_TrueStaticIP.ConfigApply"
					+ "\nExecution succeed. ");
			LOGGER.info("**********************************************************************************");

			String commandPath = DeviceModeHandler.isBusinessClassDevice(device)
					? BroadBandTestConstants.PATH_BUSINESSGW_CCSP_BUS_CLIENT_TOOL
					: BroadBandTestConstants.PATH_BUSSINESSCLASS_CCSP_BUS_CLIENT_TOOL;
			response = tapEnv.executeCommandUsingSsh(device,
					commandPath + BroadBandCommandConstants.COMMAND_APPLY_STATIC_IP_CONF);
			status = response.contains(BroadBandTestConstants.RESPONSE_EXECUTION_SUCCEED);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Applied the configuration for static IP setting");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "The result obtained from snmp command is not  same as the wan ip obtained from script in step 4 ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Wait for a 3 minutes  for statics to start routing. and Verify the  value of static IPv4 address using SNMP ");
			LOGGER.info("STEP 6: ACTION : Execute snmpwalk using mib .1.3.6.1.4.1.17270.50.2.1.4.6.0");
			LOGGER.info(
					"STEP 6: EXPECTED : The result obtained from snmp command should be same as the wan ip obtained from script in step 4");
			LOGGER.info("**********************************************************************************");

			// Execute SNMP get operation for the MIB dkbRgDeviceConfigStaticIp
			// (.1.3.6.1.4.1.17270.50.2.1.4.6.0)
			tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getOid(),
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getTableIndex());
			LOGGER.info("SNMP command output for dkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0) : "
					+ response);

			if (CommonMethods.isNotNull(response)) {
				// Validate whether the IP address returned is valid.
				status = CommonMethods.isIpv4Address(response)
						&& response.trim().equalsIgnoreCase(wan_ip_address.trim());
				errorMessage = "The IP address returned is not in a proper format. Actual: " + response;

			} else {
				errorMessage = "The device has returned inappropriate value for the dkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0). Actual: "
						+ response;
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : The result obtained from snmp command is same as the wan ip obtained from script in step 4 ");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s7";
			errorMessage = "The result obtained from webpa command is not  same as the wan ip obtained from script in step 4 ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify the ccurrent  value of static IPv4 address using webpa ");
			LOGGER.info(
					"STEP 7: ACTION : Execute webpa get on dmcli eRT getv Device.X_CISCO_COM_TrueStaticIP.IPAddressC");
			LOGGER.info(
					"STEP 7: EXPECTED : The result obtained from webpa command should be same as the wan ip obtained from script in step 4"
							+ "CR component name is: eRT.com.cisco.spvtg.ccsp.CR" + "    subsystem_prefix eRT."
							+ "    getv from/to component(eRT.com.cisco.spvtg.ccsp.pam): Device.X_CISCO_COM_TrueStaticIP.IPAddress"
							+ "    Execution succeed."
							+ "    Parameter    1 name: Device.X_CISCO_COM_TrueStaticIP.IPAddress"
							+ "                   type:     string,    value: 198.0.186.150");
			LOGGER.info("**********************************************************************************");

			// Execute the command dmcli eRT getv Device.X_CISCO_COM_TrueStaticIP.IPAddress
			// by SSHing to the device and
			// get the static WAN IP address

			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_STATIC_WAN_IP);

			if (CommonMethods.isNotNull(response)) {

				// Validate whether the IP address from the dmcli command and from SNMP are the
				// same
				status = CommonMethods.isIpv4Address(response)
						&& response.trim().equalsIgnoreCase(wan_ip_address.trim());

				errorMessage = "The static WAN IPv4 address from the dmcli command and SNMP are different. Actual: "
						+ response;

			} else {
				status = false;
				errorMessage = "The static WAN IPv4 address from the dmcli command is invalid. Actual: " + response;
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : The result obtained from webpa is  same as the wan ip obtained from script in step 4");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "Failed to connect a client to 2.4ghz or 5ghz SSID ";
			status = false;
			Dut connectedDevice = null;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Connect a wifi capable client to 2.4ghz and 5ghz client ");
			LOGGER.info("STEP 8: ACTION : Connect a wifi capable client to 2.4ghz or 5ghz SSID of client ");
			LOGGER.info("STEP 8: EXPECTED : Client should successfully connect to gateway SSID ");
			LOGGER.info("**********************************************************************************");
			try {
				connectedDevice = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				errorMessage = "Exception while trying to connect to 2.4ghz/5ghz device. " + e.getMessage();
				LOGGER.error(errorMessage);

			}
			status = null != connectedDevice;
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Connected the client with mac " + connectedDevice.getHostMacAddress()
						+ " to gateway SSID");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "Ping failed from connected client ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : ping the static IP of router  from connected client ");
			LOGGER.info("STEP 9: ACTION : Execute ping 198.0.186.150");
			LOGGER.info("STEP 9: EXPECTED : Ping should be successful");
			LOGGER.info("**********************************************************************************");

			String pingCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(
					((Device) connectedDevice).getOsType().equalsIgnoreCase(
							BroadBandConnectedClientTestConstants.OS_LINUX) ? BroadBandCommandConstants.CMD_PING_LINUX
									: BroadBandCommandConstants.CMD_PING_WINDOWS,
					BroadBandTestConstants.SINGLE_SPACE_CHARACTER, wan_ip_address);

			LOGGER.info("Going to execute the command " + pingCommand);

			String pingResponse = tapEnv.executeCommandOnOneIPClients(connectedDevice, pingCommand);

			if (CommonUtils.isNotEmptyOrNull(pingResponse)
					&& !pingResponse.contains(BroadBandTestConstants.PING_UNREACHABLE)
					&& !pingResponse.contains(BroadBandTestConstants.PING_FAILURE)
					&& !pingResponse.contains(BroadBandTestConstants.PING_LOSS_LINUX)
					&& !pingResponse.contains(BroadBandTestConstants.PING_LOSS)) {

				status = true;

			}

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : ping to " + wan_ip_address + " from connected device "
						+ connectedDevice.getHostMacAddress() + " is successful");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "there is no internet connectivity in connected client ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Check the internet connectivity of  connected client ");
			LOGGER.info(
					"STEP 10: ACTION : Execute command  curl --connect-timeout 20 --head -4 https://www.google.com\" in connected client ");
			LOGGER.info("STEP 10: EXPECTED : \"200 OK \"should be obtained ");
			LOGGER.info("**********************************************************************************");

			String command = ((Device) connectedDevice).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
									.replace("<INTERFACE>", BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
			response = tapEnv.executeCommandOnOneIPClients(connectedDevice, command);
			status = (CommonMethods.isNotNull(response)
					&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));

			if (status) {
				LOGGER.info("STEP 10: ACTUAL : There is internet connectivity for connected client "
						+ connectedDevice.getHostMacAddress() + " when the gateway is assigned with static IP "
						+ wan_ip_address);
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			errorMessage = "Failed to factory reset the device ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 11: DESCRIPTION : Factory reset the device using Webpa and wait till the device comes online");
			LOGGER.info(
					"STEP 11: ACTION : Execute set webpa command Device.X_CISCO_COM_DeviceControl.FactoryReset with string value  \"Router,Wifi,VoIP,Dect,MoCA\"");
			LOGGER.info("STEP 11: EXPECTED : Factory reset is completed and the device gets rebootes.");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);

			if (status) {
				LOGGER.info("STEP 11: ACTUAL :Factory reset is completed and the device got rebooted");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s12";
			errorMessage = "Failed to reactivate the device  after factory reset";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Reactivate the gateway device after facory rseet ");
			LOGGER.info("STEP 12: ACTION : Execute a webpa sequence for reactivation");
			LOGGER.info("STEP 12: EXPECTED : Device should be reactivated successfully");
			LOGGER.info("**********************************************************************************");

			try {
				BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
				status = true;
			} catch (Exception e) {
				status = false;
				LOGGER.error(errorMessage + e.getMessage());
			}

			if (status) {
				LOGGER.info("STEP 12: ACTUAL : Successfully reactivated the device after factory reset");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s13";
			errorMessage = "The current value of IPv4 address obtained using snmp after factory reset is not null.";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Verify the current  value of static IPv4 address using SNMP ");
			LOGGER.info("STEP 13: ACTION : Execute snmpwalk using mib .1.3.6.1.4.1.17270.50.2.1.4.6.0");
			LOGGER.info(
					"STEP 13: EXPECTED : The current  value should be null .All static IP settings should be erased on a factory reset");
			LOGGER.info("**********************************************************************************");

			// Execute SNMP get operation for the MIB dkbRgDeviceConfigStaticIp
			// (.1.3.6.1.4.1.17270.50.2.1.4.6.0)
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getOid(),
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getTableIndex());
			LOGGER.info("SNMP command output for dkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0) : "
					+ response);

			status = CommonMethods.isNull(response);
			if (status) {
				LOGGER.info(
						"STEP 13: ACTUAL : The static IP obtained using SNMP after factory reset is null as expected ."
								+ "All static IP settings got erased on a factory reset");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s14";
			errorMessage = "The current  value of IPv4 address obtained using webpa after factory reset is not null.";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : Verify thecurrent  value of static IPv4 address using webpa ");
			LOGGER.info(
					"STEP 14: ACTION : Execute webpa get on dmcli eRT getv Device.X_CISCO_COM_TrueStaticIP.IPAddressC");
			LOGGER.info(
					"STEP 14: EXPECTED : The current value should be null .All static IP settings should be erased on a factory reset"
							+ " name: Device.X_CISCO_COM_TrueStaticIP.IPAddress"
							+ "                   type:     string,    value:");
			LOGGER.info("**********************************************************************************");

			// Execute the command dmcli eRT getv Device.X_CISCO_COM_TrueStaticIP.IPAddress
			// by SSHing to the device and
			// get the static WAN IP address

			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_STATIC_WAN_IP);
			status = CommonMethods.isNull(response);

			if (status) {
				LOGGER.info(
						"STEP 14: ACTUAL : The static IP obtained using webpa after factory reset is null as expected."
								+ "All static IP settings got erased on a factory reset");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-STATIC-IP-1000");
	}

}
