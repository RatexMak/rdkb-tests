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
package com.automatics.rdkb.tests.wifi.connectedclients;

import java.util.HashMap;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandParentalControlParameter;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.ParentalControlBlockMethod;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.parentalcontrol.BroadBandParentalControlUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;

public class BroadbandConnectedDeviceListTest extends AutomaticsTestBase {

	/**
	 * Verify the functionality of LAN client after CM reboot in bridge mode.
	 * <ol>
	 * <li>Get an Ethernet connected client</li>
	 * <li>Verify the Bridge mode status via WebPA</li>
	 * <li>Verify the IPv4 Address is retrieved from the client connected to
	 * Ethernet</li>
	 * <li>Verify the IPv6 Address is retrieved from the client connected to
	 * Ethernet</li>
	 * <li>Verify the internet is accessible in the client connected to Ethernet
	 * using IPV4 Interface</li>
	 * <li>Verify the internet is accessible in the client connected to Ethernet
	 * using IPV6 Interface</li>
	 * <li>Enable the Bridge mode via WEB PA</li>
	 * <li>Verify the Bridge mode status via WebPA</li>
	 * <li>Verify the IPv4 Address is retrieved from the client connected to
	 * Ethernet</li>
	 * <li>Verify the IPv6 Address is retrieved from the client connected to
	 * Ethernet after enabling bridge mode</li>
	 * <li>Verify the internet is accessible in the client connected to Ethernet
	 * using IPV4 Interface after enabling bridge mode</li>
	 * <li>Verify the internet is accessible in the client connected to Ethernet
	 * using IPV6 Interface after enabling bridge mode</li>
	 * <li>Reboot the Gateway device and wait for IP acquisition</li>
	 * <li>Verify the Bridge mode status via WebPA</li>
	 * <li>Verify the IPv4 Address is retrieved from the client connected to
	 * Ethernet</li>
	 * <li>Verify the IPv6 Address is retrieved from the client connected to
	 * Ethernet after enabling reboot</li>
	 * <li>Verify the internet is accessible in the client connected to Ethernet
	 * using IPV4 Interface after enabling reboot</li>
	 * <li>Verify the internet is accessible in the client connected to Ethernet
	 * using IPV6 Interface after enabling reboot</li>
	 * <li>Disable and verify the bridge mode via WEBPA</li>
	 * <li>Verify whether device is not rebooted by disabling the Bridge mode</li>
	 * <li>Verify the Bridge mode status via WebPA</li>
	 * <li>Verify the IPv4 Address is retrieved from the client connected to
	 * Ethernet</li>
	 * <li>Verify the IPv6 Address is retrieved from the client connected to
	 * Ethernet after disabling bridge mode</li>
	 * <li>Verify the internet is accessible in the client connected to Ethernet
	 * using IPV4 Interface after disabling bridge mode</li>
	 * <li>Verify the internet is accessible in the client connected to Ethernet
	 * using IPV6 Interface after disabling bridge mode</li>
	 * <li>
	 * </ol>
	 * 
	 * @param device
	 * @author Parvathy P L
	 * @refactor Athira
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-BRIDGE-MODE-1004")
	public void testBridgeModeFunctionality(Dut device) {

		String testCaseId = "TC-RDKB-BRIDGE-MODE-104";
		// Test step number
		String stepNum = "";
		// String to store the error message
		String errorMessage = null;
		// String to store the test case status
		boolean status = false;

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-BRIDGE-MODE-1004");
		LOGGER.info("TEST DESCRIPTION: Verify the functionality  of LAN client after CM reboot in bridge mode.");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Get an Ethernet connected client");
		LOGGER.info("2. Verify the Bridge mode status via WebPA");
		LOGGER.info("3. Verify the IPv4 Address is retrieved  from the client connected to Ethernet");
		LOGGER.info("4.Verify the IPv6 Address is retrieved  from the client connected to Ethernet");
		LOGGER.info("5. Verify the internet is accessible in the client connected to Ethernet using IPV4 Interface");
		LOGGER.info("6. Verify the internet is accessible in the client connected to Ethernet using IPV6 Interface");
		LOGGER.info("7. Enable the Bridge mode via WEB PA");
		LOGGER.info("8. Verify the Bridge mode status via WebPA");
		LOGGER.info("9. Verify the IPv4 Address is retrieved  from the client connected to Ethernet");
		LOGGER.info(
				"10. Verify the IPv6 Address is retrieved  from the client connected to Ethernet after enabling bridge mode");
		LOGGER.info(
				"11. Verify the internet is accessible in the client connected to Ethernet using IPV4 Interface after enabling bridge mode");
		LOGGER.info(
				"12. Verify the internet is accessible in the client connected to Ethernet using IPV6 Interface after enabling bridge mode");
		LOGGER.info("13. Reboot the Gateway device and wait for IP acquisition");
		LOGGER.info("14.Verify the Bridge mode status via WebPA");
		LOGGER.info("15.Verify the IPv4 Address retrieved  from the client connected to Ethernet after reboot");
		LOGGER.info(
				"16. Verify the IPv6 Address is retrieved  from the client connected to Ethernet after enabling reboot");
		LOGGER.info(
				"17. Verify the internet is accessible in the client connected to Ethernet using IPV4 Interface after enabling reboot");
		LOGGER.info(
				"18. Verify the internet is accessible in the client connected to Ethernet using IPV6 Interface after enabling reboot");
		LOGGER.info("19.Disable and verify the bridge mode via WEBPA");
		LOGGER.info("20.Verify whether device is  not rebooted by disabling the Bridge mode");
		LOGGER.info("21.Verify the Bridge mode status via WebPA");
		LOGGER.info("22. Verify the IPv4 Address is retrieved  from the client connected to Ethernet");
		LOGGER.info(
				"23. Verify the IPv6 Address is retrieved  from the client connected to Ethernet after disabling bridge mode");
		LOGGER.info(
				"24. Verify the internet is accessible in the client connected to Ethernet using IPV4 Interface after disabling bridge mode");
		LOGGER.info(
				"25. Verify the internet is accessible in the client connected to Ethernet using IPV6 Interface after disabling bridge mode");
		LOGGER.info("#######################################################################################");
		try {
			stepNum = "s1";
			errorMessage = "Failed to get an Ethernet connected client";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Get an Ethernet connected client");
			LOGGER.info("STEP 1: ACTION : Get a device of capability Ethernet from connected device list");
			LOGGER.info("STEP 1: EXPECTED : Device should be connected to Ethernet");
			LOGGER.info("**********************************************************************************");
			Dut connectedClientEthernetDevice = null; // connected device to be verified
			try {
				connectedClientEthernetDevice = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv,
						device);
				status = (null != connectedClientEthernetDevice);
				if (status) {
					LOGGER.info("STEP 1: ACTUAL:Obtained Ethernet connected client device");
				} else {
					LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
				}
			} catch (TestException exception) {
				errorMessage = "Exception occured while verifying the ethernet connected devices : "
						+ exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s2";
			errorMessage = "BridgeMode is found to be enabled in default state via WebPA";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify the Bridge mode status via WebPA");
			LOGGER.info(
					"STEP 2: ACTION : Execute WebPA command to get the Bridge mode status using WebPA parameter Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode");
			LOGGER.info("STEP 2: EXPECTED : Device should return the current mode and the value should be  \"router\"");
			LOGGER.info("**********************************************************************************");
			try {
				status = BroadBandCommonUtils.verifyDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
				if (status) {
					LOGGER.info(
							"STEP 2: ACTUAL:Current mode of operation is returned as 'router' from webpa parameter Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode");
				} else {
					LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
				}
			} catch (TestException exception) {
				errorMessage = "Exception occured while validating Bridge mode status via webpa with message:"
						+ exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 3-6 : Verify the client connected to ethernet has valid IPv4
			 * Address,ipv6 address and internet connectivity
			 */

			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionForEthernetClient(device, testCaseId,
					connectedClientEthernetDevice, BroadBandTestConstants.CONSTANT_3);

			stepNum = "s7";
			status = false;
			errorMessage = "Not able to set the value of " + BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS
					+ " to bridege-static";
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION: Enable Bridge mode using webpa ");
			LOGGER.info("STEP 7: ACTION: Web PA server response should contain the   message as success.");
			LOGGER.info(
					"STEP 7: EXPECTED: Value of Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode must be set to \"bridge-static\" successfully");
			LOGGER.info(
					"************************************************************************************************");
			try {

				status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
						BroadBandTestConstants.CONSTANT_BRIDGE_STATIC, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

				if (status) {
					LOGGER.info(
							"STEP 7: ACTUAL: Value of Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode is set to \"bridge-static\" successfully");
				} else {
					LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
				}
			} catch (TestException exception) {
				LOGGER.error("Exception occurred during execution => " + exception.getMessage());
				errorMessage = errorMessage + exception.getMessage();
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// waiting for 120 sec for the web pa parameter to set
			tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);

			stepNum = "s8";
			errorMessage = "Bridge mode is found to be disabled even after enabling the bridge mode via WebPa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify the Bridge mode status via WebPA");
			LOGGER.info(
					"STEP 8: ACTION : Execute WebPA command to get the Bridge mode status using Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode.");
			LOGGER.info(
					"STEP 8: EXPECTED : Device should return the current mode and the value should be  \"bridge-static\"");
			LOGGER.info("**********************************************************************************");
			try {
				status = BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device);
				if (status) {
					LOGGER.info(
							"STEP 8: ACTUAL:Current mode of operation is obtained as 'bridge-static' from webpa parameter after enabling the bridge mode");

				} else {
					LOGGER.error("STEP 8: ACTUAL :" + errorMessage);

				}
			} catch (TestException exception) {
				errorMessage = "Failed to validate the BridgeMode status due to exception:" + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// wait 8 mins for the device to turn into bridge mode
			LOGGER.info("waiting for Eight minutes after enabling Bridge mode");
			tapEnv.waitTill(RDKBTestConstants.EIGHT_MINUTE_IN_MILLIS);

			/**
			 * Step 9-12 : Verify the client connected to ethernet has valid IPv4
			 * Address,ipv6 address and internet connectivity
			 */

			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionForEthernetClient(device, testCaseId,
					connectedClientEthernetDevice, BroadBandTestConstants.CONSTANT_9);

			stepNum = "s13";
			status = false;
			errorMessage = "Failed to reboot the device successfully";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION :  Reboot the Gateway device and wait for IP acquisition");
			LOGGER.info("STEP 13: ACTION : Execute Command /sbin/reboot");
			LOGGER.info("STEP 13: EXPECTED : Device should be rebooted");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Successfully rebooted the STB and able to access the device");
			} else {
				LOGGER.error("STEP 13: ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// wait after REBOOT
			LOGGER.info("Waiting for three minutes for Webpa service to start running after reboot");
			tapEnv.waitTill(AutomaticsConstants.THREE_MINUTES);

			stepNum = "s14";
			errorMessage = "Bridge mode is found to be disabled after the device reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : Verify the Bridge mode status via WebPA");
			LOGGER.info(
					"STEP 14: ACTION : Execute WebPA command to get the Bridge mode status using Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode.");
			LOGGER.info(
					"STEP 14: EXPECTED : Device should return the current mode and the value should be  \"bridge-static\"");
			LOGGER.info("**********************************************************************************");
			try {
				status = BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device);
			} catch (TestException exception) {
				errorMessage = "Failed to validate the BridgeMode status due to exception:" + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 14: ACTUAL:Current mode of operation is obtained as 'bridge-static' from webpa parameter after the device reboot");

			} else {
				LOGGER.error("STEP 14: ACTUAL :" + errorMessage);

			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 15-18 : Verify the client connected to ethernet has valid IPv4
			 * Address,ipv6 address and internet connectivity
			 */

			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionForEthernetClient(device, testCaseId,
					connectedClientEthernetDevice, BroadBandTestConstants.CONSTANT_15);

			stepNum = "s19";
			status = false;
			errorMessage = "Failed to change the device to router mode from bridge mode using "
					+ BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 19: DESCRIPTION: Disable Bridge mode using webpa");
			LOGGER.info(
					"STEP 19: ACTION: Set web pa parameter Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode to value 'router'");
			LOGGER.info(
					"STEP 19: EXPECTED: Value of \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\" must be set to \"router\"");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandCommonUtils.setDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
			} catch (TestException exception) {
				LOGGER.error("caught TestException with message - " + exception.getMessage());
				errorMessage = errorMessage + exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 19: ACTUAL: Disabled Bridge mode using WebPa");
			} else {
				LOGGER.error("STEP 19: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// wait for device to turn into router mode
			LOGGER.info("waiting for Eight minutes for disabling bridge mode");
			tapEnv.waitTill(RDKBTestConstants.EIGHT_MINUTE_IN_MILLIS);

			stepNum = "s20";
			errorMessage = "STB is not accessible after Bridge to Router transition, may be due to device reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 20: DESCRIPTION : Verify whether device is not rebooted by disabling the Bridge mode");
			LOGGER.info(
					"STEP 20: ACTION : Check whether the device rebooted after disabling the Bridge mode to ensure that there is no crash observed during the mode change from Bridge to Router.");
			LOGGER.info(
					"STEP 20: EXPECTED : Device should not reboot during the mode change from Bridge to Router mode.");
			LOGGER.info("**********************************************************************************");
			status = !CommonMethods.isSTBRebooted(tapEnv, device, AutomaticsConstants.THIRTY_SECOND_IN_MILLIS,
					AutomaticsConstants.CONSTANT_5);
			if (status) {
				LOGGER.info("STEP 20: ACTUAL:Device is not rebooted after mode transition from Bridge to Router");
			} else {
				LOGGER.error("STEP 20: ACTUAL:" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// in the above step we waited for five minutes for bridge mode to be disabled
			// .. wait for three more
			// minutes
			// for the web pa service to start after turning off bridge mode
			LOGGER.info("Waiting for three minutes for Webpa service to start running");
			tapEnv.waitTill(AutomaticsConstants.THREE_MINUTES);

			stepNum = "s21";
			errorMessage = "Device is found to be in BridgeMode via WebPA while it is expected to be in RouterMode";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 21: DESCRIPTION : Verify the Bridge mode status via WebPA");
			LOGGER.info(
					"STEP 21: ACTION : Execute WebPA command to get the Bridge mode status using Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode.");
			LOGGER.info(
					"STEP 21: EXPECTED : Device should return the current mode and the value should be  \"router\".sample response:{\"statusCode\":200,\"parameters\":[{\"name\":\"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\",\"value\":\"router\",\"dataType\":0,\"parameterCount\":1,\"message\":\"Success\"}]}");
			LOGGER.info("**********************************************************************************");
			try {
				status = BroadBandCommonUtils.verifyDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
				if (status) {
					LOGGER.info(
							"STEP 21: ACTUAL:Current mode is returned as 'router' for web pa parameter Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode");
				} else {
					LOGGER.error("STEP 21: ACTUAL: " + errorMessage);
				}
			} catch (TestException exception) {
				errorMessage = errorMessage + " " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 22-25 : Verify the client connected to ethernet has valid IPv4
			 * Address,ipv6 address and internet connectivity
			 */

			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionForEthernetClient(device, testCaseId,
					connectedClientEthernetDevice, BroadBandTestConstants.CONSTANT_22);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error("Caught exception with message " + e.getMessage());
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			if (BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device)) {
				BroadBandPostConditionUtils.executePostConditionToDisableBirdgeMode(device, tapEnv, 1);
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-BRIDGE-MODE-1004");
	}

	/**
	 *
	 * Test Case : Test script to validate basic wifi parameters on router mode
	 * transition
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Change the private ssid names in both 2.4Ghz and 5Ghz channels using
	 * Device.WiFi.SSID.{i}.SSID</li>
	 * <li>Change the Default wifi channel numbers in 2.4Ghz channel using webpa
	 * parameters</li>
	 * <li>Change the Default wifi channel numbers in 5 Ghz channel using webpa
	 * parameters</li>
	 * <li>Get the remoter access status and change the values on vice versa for
	 * both http and https using webpa parameters</li>
	 * <li>Set a parental control rule by enabling
	 * \"Device.X_Comcast_com_ParentalControl.ManagedSites.Enable \" and set
	 * rules</li>
	 * <li>Verify the port forwarding feature can be enabled using WebPA</li>
	 * <li>Connect the device to the private Wi-Fi SSID and verify connection
	 * status</li>
	 * <li>Check if the wireless connected client has an IP address from the
	 * gateway</li>
	 * <li>Verify the port forwarding rule for HTTP service is added for the client
	 * connected to the gateway</li>
	 * <li>Verify whether IP tables has the port forwarding rules configured when
	 * device in Router Mode</li>
	 * <li>Verify the WAN IP Address of the gateway is retrieved successfully</li>
	 * <li>Verify the HTTP connection is successful when portforwarding is
	 * configured</li>
	 * <li>Change the security mode of wifi in 2.4Ghz band to WPA2 Enterprise using
	 * \ "Device.WiFi.AccessPoint.1.Security.ModeEnabled \".</li>
	 * <li>Change the security mode of wifi in 5Ghz band to WPA2 Enterprise using \
	 * "Device.WiFi.AccessPoint.1.Security.ModeEnabled \"</li>
	 * <li>Enable Bridge mode using webpa command
	 * \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\"</li>
	 * <li>verify whether Bridge mode is enabled using SNMP MIB
	 * \"1.3.6.1.4.1.17270.50.2.3.2.1.1.32\"</li>
	 * <li>Verify whether IP tables doesn’t have the port forwarding rules configred
	 * in bridge mode</li>
	 * <li>Verify Whether IP tables restore doesn’t fail when device in bridge
	 * mode</li>
	 * <li>Disable Bridge mode using webpa command
	 * \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\"</li>
	 * <li>Verify whether Bridge mode disabled using SNMP MIB
	 * 1.3.6.1.4.1.17270.50.2.3.2.1.1.32</li>
	 * <li>Verify the Private SSID names in both 2.4Ghz and 5Ghz bands using
	 * Device.WiFi.SSID.{i}.SSID</li>
	 * <li>Verify the channel number in 2.4Ghz band using
	 * \"Device.WiFi.Radio.{i}.Channel\"</li>
	 * <li>Verify the channel number in 5Ghz band using
	 * Device.WiFi.Radio.{i}.Channel</li>
	 * <li>Verify the values of remote access status for both http and https</li>
	 * <li>Verify the security mode of private wifi in 2.4Ghz band using
	 * Device.WiFi.AccessPoint.10001.Security.ModeEnabled</li>
	 * <li>Verify the security mode of private wifi in 5Ghz band using
	 * Device.WiFi.AccessPoint.10101.Security.ModeEnabled</li>
	 * <li>Verify the persistence of parental control rule</li>
	 * <li>Verify whether IP tables has the port forwarding rules configured when
	 * device in Router Mode</li>
	 * <li>Verify the port forwarding rule for HTTP service can be deleted
	 * successfully</li>
	 * <li>Verify the port forwarding feature can be disabled using WebPA</li>
	 * <li>POST-CONDITION 1 : Change the Device back to router mode if it is in
	 * Bridge mode</li>
	 * <li>POST-CONDITION 2 : Change the HTTP remote access statuses since we have
	 * altered them in STEP 4</li>
	 * <li>POST-CONDITION 3 : Change the channel number in 2Ghz and 5Ghz bands to
	 * initial values</li>
	 * <li>POST-CONDITION 4 : Disable auto channel Enable status in both 2.4Ghz and
	 * 5Ghz bands</li>
	 * <li>POST-CONDITION 5 : Change the security mode in both 2.4Ghz and 5Ghz bands
	 * since we have updated them in STEP5 and STEP6</li>
	 * <li>POST-CONDITION 6 : Delete the new parental control rule created</li>
	 * <li>POST-CONDITION 7 : Verify the port forwarding rule for HTTP service can
	 * be deleted successfully</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Revanth.K , Gnanaprakasham S,Joseph M
	 * @refactor Govardhan
	 *
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-BRIDGE-MODE-1002")
	public void testToVerifyUserConfigPersistencyAcrossModeTransitions(Dut device) {

		// Variable to store test case id
		String testId = "TC-RDKB-BRIDGE-MODE-002";
		// Variable to store test step number
		String testStepNumber = "s1";
		// boolean variable to store the status of each step
		boolean status = false;
		// String variable to store the error message
		String errorMessage = null;
		// string to store the channel number to which 2Ghz wifi band is changed
		String updatedWifiChannelNumberIn2GhzBand = null;
		// string to store the channel number to which 5Ghz wifi band is changed
		String updatedWifiChannelNumberIn5GhzBand = null;
		// string to store the value to which http remote access is changed
		String updatedValueofhttpRemoteAccess = null;
		// string to store the value to which https remote access is changed
		String updatedValueofhttpsRemoteAccess = null;
		// string to store updated parental control rule
		String newParameterCreatedOnSettingParentalControlRule = null;
		// Variable to store private ssid status for 2 GHz
		boolean statusFor2GhzParameter = false;
		// Variable to store private ssid status for 5 GHz
		boolean statusFor5GhzParameter = false;
		// Map to store the parameters and values for multiple webpa execution
		HashMap<String, String> parameterValueMap = new HashMap<String, String>();
		// String variable to store the current channel number in 2Ghz band
		String currentWifiChannelNumberIn2Ghz = null;
		// String variable to store the current channel number in 5Ghz band
		String currentWifiChannelNumberIn5Ghz = null;
		Dut connectedClientDevice = null; // connected device to be verified
		String portForwardingTableAddRowResponse = null;
		BroadBandResultObject result = null; // stores test result and error message
		boolean isRuleDeleted = false;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-BRIDGE-MODE-1002");
			LOGGER.info(
					"TEST DESCRIPTION: Test to verify SSID's enabled/disabled status when bridge mode enabled/disable using SNMP");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"STEP 1: Change the private ssid names in both 2.4Ghz and 5Ghz channels using Device.WiFi.SSID.{i}.SSID");
			LOGGER.info("STEP 2: Change the Default wifi channel numbers in 2.4Ghz channel using webpa parameters ");
			LOGGER.info("STEP 3: Change the Default wifi channel numbers in 5 Ghz channel using webpa parameters");
			LOGGER.info(
					"STEP 4: Get the remoter access status and change the values on vice versa for both http and https using webpa parameters");
			LOGGER.info(
					"STEP 5: Set a parental control rule by enabling \"Device.X_Comcast_com_ParentalControl.ManagedSites.Enable \" and set rules ");
			LOGGER.info("STEP 6: Verify the port forwarding feature can be enabled using WebPA");
			LOGGER.info("STEP 7: Connect the device to the private Wi-Fi SSID and verify connection status");
			LOGGER.info("STEP 8: Check if the wireless connected client has an IP address from the gateway");
			LOGGER.info(
					"STEP 9: Verify the port forwarding rule for HTTP service is added for the client connected to the gateway");
			LOGGER.info(
					"STEP 10: Verify whether IP tables has the port forwarding rules configured when device in Router Mode");
			LOGGER.info("STEP 11 :Verify the WAN IP Address of the gateway is retrieved successfully");
			LOGGER.info("STEP 12: Verify the HTTP connection is successful when portforwarding is configured");
			LOGGER.info(
					"STEP 13: Change the security mode of wifi in 2.4Ghz band to WPA2 Enterprise using \"Device.WiFi.AccessPoint.1.Security.ModeEnabled \"");
			LOGGER.info(
					"STEP 14: Change the security mode of wifi in 5Ghz band to WPA2 Enterprise using \"Device.WiFi.AccessPoint.1.Security.ModeEnabled \"");

			LOGGER.info(
					"STEP 15: Enable Bridge mode using webpa command \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\" ");
			LOGGER.info(
					"STEP 16: verify whether Bridge mode is enabled using SNMP MIB \"1.3.6.1.4.1.17270.50.2.3.2.1.1.32\"");
			LOGGER.info(
					"STEP 17:Verify whether IP tables doesn’t have  the port forwarding rules configred in bridge mode");
			LOGGER.info("STEP 18:Verify Whether IP tables restore doesn’t fail when device in bridge mode");
			LOGGER.info(
					"STEP 19: Disable Bridge mode using webpa  command \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\"");
			LOGGER.info(
					"STEP 20: Verify whether Bridge mode disabled using SNMP MIB  1.3.6.1.4.1.17270.50.2.3.2.1.1.32 ");
			LOGGER.info(
					"STEP 21: Verify the Private SSID names in both 2.4Ghz and 5Ghz bands using Device.WiFi.SSID.{i}.SSID");
			LOGGER.info("STEP 22: Verify the channel number in 2.4Ghz band using \"Device.WiFi.Radio.{i}.Channel\" ");
			LOGGER.info("STEP 23: Verify the channel number in 5Ghz band using Device.WiFi.Radio.{i}.Channel ");
			LOGGER.info("STEP 24: Verify the values of remote access status for both http and https ");
			LOGGER.info(
					"STEP 25: Verify the security mode of private wifi in 2.4Ghz band using Device.WiFi.AccessPoint.10001.Security.ModeEnabled ");
			LOGGER.info(
					"STEP 26: Verify the security mode of private wifi in 5Ghz band using  Device.WiFi.AccessPoint.10101.Security.ModeEnabled ");
			LOGGER.info("STEP 27: Verify the persistence of parental control rule  ");
			LOGGER.info(
					"STEP 28: Verify whether IP tables has the port forwarding rules configured when device in Router Mode ");
			LOGGER.info("STEP 29: Verify the port forwarding rule for HTTP service can be deleted successfully");
			LOGGER.info("STEP 30: Verify the port forwarding feature can be disabled using WebPA");
			LOGGER.info(" POST-CONDITION 1 : Change the Device back to router mode if it is in Bridge mode");
			LOGGER.info(
					" POST-CONDITION 2 : Change the HTTP remote access statuses since we have altered them in STEP 4");
			LOGGER.info(" POST-CONDITION 3 : Change the channel number in 2Ghz and 5Ghz bands to initial values");
			LOGGER.info(" POST-CONDITION 4 : Disable auto channel Enable status in both 2.4Ghz and 5Ghz bands");
			LOGGER.info(
					" POST-CONDITION 5 : Change the security mode in both 2.4Ghz and 5Ghz bands since we have updated them in STEP5 and STEP6");
			LOGGER.info(" POST-CONDITION 6 : Delete the new parental control rule created");
			LOGGER.info(
					" POST-CONDITION 7 : Verify the port forwarding rule for HTTP service can be deleted successfully");

			/**
			 * step 1: Change the private ssid names in both 2.4Ghz and 5Ghz channels.
			 * Expected : ssid names must be changed successfully.
			 */
			testStepNumber = "s1";
			errorMessage = "Not able to set private SSID name using Device.WiFi.SSID.{i}.SSID for ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION: Change the private ssid names in both 2.4Ghz and 5Ghz channels using Device.WiFi.SSID.{i}.SSID");
			LOGGER.info(
					"STEP 1: ACTION: Set the values of parameters Device.WiFi.SSID.10001.SSID ,Device.WiFi.SSID.10101.SSID to Hello_2.4 ,Hello_5");
			LOGGER.info(
					"STEP 1: EXPECTED: Values of the SSID's must be updated successfully.Get the values of  Device.WiFi.SSID.{i}.SSID and check whether they are updated successfully");
			LOGGER.info(
					"************************************************************************************************");
			try {
				parameterValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.PRIVATEWIFI_NAME_FOR_2GHZ_BAND);
				parameterValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.PRIVATEWIFI_NAME_FOR_5GHZ_BAND);
				status = BroadBandWebPaUtils.executeMultipleWebpaParametersSet(device, tapEnv, parameterValueMap,
						WebPaDataTypes.STRING.getValue());
				LOGGER.info("waiting for ONE minutes after setting wifi related parameter");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			} catch (TestException exception) {
				errorMessage += exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Successfully changed private SSID name for both 2.4 and 5 GHz :" + status);
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * step 2:Change the Default wifi channel numbers in 2.4Ghz channel Expected :
			 * Channel number must be changed successfully
			 * 
			 */
			testStepNumber = "s2";
			errorMessage = "Unable to change the channel number for private wifi in 2Ghz band";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 2:DESCRITPION: Change the Default wifi channel numbers in 2.4Ghz channel using webpa parameters ");
			LOGGER.info(
					"STEP 2: ACTION: Set the values of webpa parameter  Device.WiFi.Radio.10000.AutoChannelEnable to false and Device.WiFi.Radio.10000.Channel to A random Applicable value");
			LOGGER.info(
					"STEP 2: EXPECTED: Wifi channel in 2.4 Ghz must be successfully changed from default channel numbers");
			LOGGER.info(
					"************************************************************************************************");
			try {
				currentWifiChannelNumberIn2Ghz = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ);
				if (CommonMethods.isNull(currentWifiChannelNumberIn2Ghz)) {
					LOGGER.error(
							"Current wifi channel number is obtained as NULL for parameter -Device.WiFi.Radio.10000.Channel");
				}
				updatedWifiChannelNumberIn2GhzBand = BroadBandWiFiUtils.changePrivateWifiChannelToNonDefaultRandomValue(
						device, tapEnv, BroadBandTestConstants.WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				status = CommonMethods.isNotNull(updatedWifiChannelNumberIn2GhzBand);
			} catch (TestException exception) {
				LOGGER.error("TestException caught with message " + exception.getMessage());
				errorMessage += exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 2:ACTUAL:Successfully changed the channel number to :"
						+ updatedWifiChannelNumberIn2GhzBand + " for 2.4 GHz");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * step 3:Change the Default wifi channel numbers in 5Ghz channel Expected :
			 * Channel number must be changed successfully
			 * 
			 */
			testStepNumber = "s3";
			errorMessage = "Unable to change the channel number for private wifi in 5Ghz band";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Change the Default wifi channel numbers in 5 Ghz channel using webpa parameters");
			LOGGER.info(
					"STEP 3: ACTION: Set the values of webpa parameter  Device.WiFi.Radio.10100.AutoChannelEnable to false and Device.WiFi.Radio.10100.Channel to A random Applicable value");
			LOGGER.info(
					"STEP 3: EXPECTED: Wifi channel in 5 Ghz must be successfully changed from default channel numbers ");
			LOGGER.info(
					"************************************************************************************************");
			try {
				currentWifiChannelNumberIn5Ghz = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
				if (CommonMethods.isNull(currentWifiChannelNumberIn2Ghz)) {
					LOGGER.error(
							"Current wifi channel number is obtained as NULL for parameter -Device.WiFi.Radio.10100.Channel");
				}
				updatedWifiChannelNumberIn5GhzBand = BroadBandWiFiUtils.changePrivateWifiChannelToNonDefaultRandomValue(
						device, tapEnv, BroadBandTestConstants.WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				status = CommonMethods.isNotNull(updatedWifiChannelNumberIn5GhzBand);
			} catch (TestException exception) {
				LOGGER.error("TestException caught with message " + exception.getMessage());
				errorMessage = exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 3:ACTUAL:Successfully changed the channel number to :"
						+ updatedWifiChannelNumberIn5GhzBand + " for 5 GHz");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * step 4 : Change the default values of remote access statuses. Expected :
			 * values must be changed successfully
			 */
			testStepNumber = "s4";
			errorMessage = "Not able to change remote access status for http and https using webpa parameters";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION: Get the remote access status and change the values on vice versa for both http and https using webpa parameters");
			LOGGER.info(
					"STEP 4: ACTION: Toggle the values of the parameters Device.UserInterface.X_CISCO_COM_RemoteAccess.HttpEnable,Device.UserInterface.X_CISCO_COM_RemoteAccess.HttpsEnable");
			LOGGER.info("STEP 4: EXPECTED: Values of the parameters must be changed successfully");
			LOGGER.info(
					"************************************************************************************************");
			try {
				updatedValueofhttpRemoteAccess = BroadBandWebPaUtils.toggleBooleanWebpaParameterValue(device,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_HTTP_REMOTE_ACCESSSTATUS, tapEnv);
				LOGGER.info("Changed the http remote access status to :" + updatedValueofhttpRemoteAccess);
				updatedValueofhttpsRemoteAccess = BroadBandWebPaUtils.toggleBooleanWebpaParameterValue(device,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_HTTPS_REMOTE_ACCESSSTATUS, tapEnv);
				LOGGER.info("Changed the https remote access status to :" + updatedValueofhttpsRemoteAccess);
				status = CommonMethods.isNotNull(updatedValueofhttpRemoteAccess)
						&& CommonMethods.isNotNull(updatedValueofhttpsRemoteAccess);
			} catch (TestException exception) {
				LOGGER.error("TestException caught with message " + exception.getMessage());
				errorMessage = errorMessage + exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 4:ACTUAL:Toggled the values of remote access parameters" + status);
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * 
			 * step 5 : Set a parental Control rule. Expected : parental control rule must
			 * be set successfully
			 * 
			 */
			testStepNumber = "s5";
			errorMessage = "Failed to set parental control rule";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Set a parental control rule");
			LOGGER.info(
					"STEP 5: ACTION: Enable parameter Device.X_Comcast_com_ParentalControl.ManagedSites.Enable and add a new table rule using Device.X_Comcast_com_ParentalControl.ManagedSites.Enable.");
			LOGGER.info(
					"STEP 5: EXPECTED: parental rule must be set successfully verify it using the parameter returned in the success message ");
			LOGGER.info(
					"************************************************************************************************");
			try {
				String blockDays = BroadBandParentalControlUtils
						.getCurrentDayToAddParentalControlRuleWhenAlwaysBlockIsDisabled(tapEnv, device);
				errorMessage = "Null value obtained for the current day which needs to added in parental Control - Managed Device Rule. Actual value Obtained: "
						+ blockDays;
				if (CommonMethods.isNotNull(blockDays)) {
					BroadBandParentalControlParameter parentalControlParam = new BroadBandParentalControlParameter(
							ParentalControlBlockMethod.URL.getValue(), BroadBandTestConstants.URL_WIKIPEDIA,
							BroadBandTestConstants.TRUE, BroadBandTestConstants.HOURS_12_AM,
							BroadBandTestConstants.HOURS_23_59_PM, blockDays);
					newParameterCreatedOnSettingParentalControlRule = BroadBandCommonUtils
							.setParentalControlParam(device, tapEnv, parentalControlParam);
					errorMessage = "Null response obtained for setting Parental Control - Managed Sites Rule.";
					if (CommonMethods.isNotNull(newParameterCreatedOnSettingParentalControlRule)) {
						status = CommonUtils.patternSearchFromTargetString(
								newParameterCreatedOnSettingParentalControlRule,
								BroadBandWebPaConstants.WEBPA_PARAM_TO_SET_A_PARENTAL_CONTROL_RULE);
						errorMessage = "Website which needs to be blocked for a specific day and time period on the connected client cannot be added to blocked sites table.";
					}
				}
			} catch (TestException exception) {
				errorMessage = errorMessage + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 5:ACTUAL: New parental control parameter is set " + status);
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 6: Verify the port forwarding feature can be enabled using WebPA
			 */
			testStepNumber = "s6";
			errorMessage = "Port Forwarding feature cannot be enabled using WebPA";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify the port forwarding feature can be enabled using WebPA");
			LOGGER.info("STEP 6: ACTION : Execute webpa command-Device.NAT.X_Comcast_com_EnablePortMapping to true");
			LOGGER.info("STEP 6: EXPECTED: Port Forwarding feature should be enabled using WebPA");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_AND_DISABLE_PORT_FORWARDING,
					WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Port Forwarding feature is enabled successfully using WebPA");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 7: Connect the device to the private Wi-Fi SSID and verify connection
			 * status
			 * 
			 */
			testStepNumber = "S7";
			errorMessage = "Unable to connect to the private 2.4GHz or 5GHz Wi-Fi Network";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 7: DESCRIPTION : Connect a client to Private 2.4GHz or  5Ghz Wi-Fi Network and verify connection status ");
			LOGGER.info(
					"STEP 7: ACTION : Connect to Private Wi-Fi using below commandsLinux :nmcli dev wifi connect <ssid> password <passwd>Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP 7: EXPECTED : Device should be connected with  Private 2.4GHz or  5GHz Wi-Fi Network");
			LOGGER.info("**********************************************************************************");
			try {
				connectedClientDevice = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
			}
			status = null != connectedClientDevice;
			String successMessage = null;
			if (status) {
				if (!DeviceModeHandler.isRPIDevice(device)) {
					successMessage = "Connected Client is assigned with a valid IPv4 Address DHCP Range";
					status = BroadBandConnectedClientUtils.verifyIISStatus(connectedClientDevice, tapEnv,
							BroadBandConnectedClientTestConstants.IIS_START_FLAG);
					successMessage = successMessage + " - IIS service is running successfully";
					errorMessage = successMessage + " - Unnable to start IIS service";
				} else {
					status = true;
				}
			}
			if (status) {
				LOGGER.info("STEP 7 ACTUAL: " + successMessage);
			} else {
				LOGGER.error("STEP 7 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 8: Check if the wireless connected client has an IP address from the
			 * gateway
			 *
			 */
			testStepNumber = "s8";
			errorMessage = "Unable to retrieve the IP Address form the cilent connected to the private Wi-Fi";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8 : DESCRIPTION : Check if the wireless connected client has an IP address from the gateway");
			LOGGER.info(
					"STEP 8 : ACTION : Get the device IPv4 address using below command Linux : ifconfig wlan0 |grep -i 'inet addr:'Windows: ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address'");
			LOGGER.info("STEP 8: EXPECTED : IP Address should be retrieved from the Wireless Connected device");
			LOGGER.info("**********************************************************************************");
			String ipAddressRetrievedFromClient = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
					device, connectedClientDevice);
			LOGGER.info("IP ADDRESS ASSIGNED TO THE CONNECTED CLIENT FROM DHCP : " + ipAddressRetrievedFromClient);
			errorMessage = "Cilent connected to the private Wi-Fi haven't received valid IP Address from Gateway";
			status = CommonMethods.isNotNull(ipAddressRetrievedFromClient)
					&& CommonMethods.isIpv4Address(ipAddressRetrievedFromClient);
			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL: Client connected to the private Wi-Fi network has got IP Address from Gateway :"
								+ ipAddressRetrievedFromClient);
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 9: Verify the port forwarding rule for HTTP service is added for the
			 * client connected to the gateway
			 *
			 */
			testStepNumber = "s9";
			errorMessage = "Null response obtained when configuring Port Forwarding Rule";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Verify the port forwarding rule for HTTP service is added for the client connected to the gateway");
			LOGGER.info(
					"STEP 9: ACTION : Execute the webpa post command wth various param to create a port forwarding rule");
			LOGGER.info("STEP 9: EXPECTED : Port Forwarding rule for HTTP service should be added successfully");
			LOGGER.info("**********************************************************************************");
			portForwardingTableAddRowResponse = BroadBandCommonUtils.configurePortForwardingRule(tapEnv, device,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.HTTP_PORT_NUMBER,
					BroadBandTestConstants.HTTP_PORT_NUMBER, BroadBandTestConstants.PROTOCOL_TCP_AND_UDP,
					ipAddressRetrievedFromClient, BroadBandTestConstants.PORT_FORWARDING_RULE_DESCRIPTION_AS_HTTP);
			if (CommonMethods.isNotNull(portForwardingTableAddRowResponse)) {
				status = CommonUtils.patternSearchFromTargetString(portForwardingTableAddRowResponse,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_CONFIGURE_PORT_FORWARDING_RULE);
				errorMessage = "Port Forwarding rule for HTTP service cannot be configured";
			}
			if (status) {
				// Waiting for the rule to apply
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				LOGGER.info("STEP 9 ACTUAL: Port Forwarding rule for HTTP service is configured successfully");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 10: Verify whether IP tables has the port forwarding rules configured
			 * when device in Router Mode
			 *
			 */
			status = false;
			testStepNumber = "s10";
			errorMessage = "IP tables doesn't have  the port forwarding rules configred in Router mode";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 10: DESCRIPTION : Verify whether IP tables has the port forwarding rules configured when device in Router Mode");
			LOGGER.info("STEP 10: ACTION : Execute the command: cat .ipt | grep -i  <LAN-IPv4 ADDRESS> ");
			LOGGER.info("STEP 10: EXPECTED : Should get the  configured portfowarding rule in router mode");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.verifyPortForwardingRuleConfigurationInIPTables(tapEnv, device,
					ipAddressRetrievedFromClient);
			if (status) {
				LOGGER.info(
						"STEP 10 ACTUAL: IP tables has the port forwarding rules configured when device in Router Mode");
			} else {
				LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 11: Check if the wireless connected client has an IP address from the
			 * gateway
			 *
			 */
			testStepNumber = "s11";
			errorMessage = "Unable to retrieve WAN IP Address of the gateway using WebPA/dmcli.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify the WAN IP Address of the gateway is retrieved successfully");
			LOGGER.info(
					"STEP 11: ACTION : Execute the command : ifconfig erouter0 to get the WAN IP Address of the gateway");
			LOGGER.info("STEP 11: EXPECTED : WAN IP Address of the gateway should be retieved successfully");
			LOGGER.info("**********************************************************************************");
			String wanIpAddress = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV4);
			LOGGER.info("WAN IP ADDRESS OF THE DEVICE: " + wanIpAddress);
			status = CommonMethods.isNotNull(wanIpAddress) && CommonMethods.isIpv4Address(wanIpAddress);
			if (status) {
				LOGGER.info("STEP 11 :ACTUAL: WAN IP Address of the gateway is retieved successfully :" + wanIpAddress);
			} else {
				LOGGER.error("STEP 11: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 12: Verify the HTTP connection is successful when portforwarding is
			 * configured
			 */
			status = false;
			testStepNumber = "s12";
			errorMessage = "HTTP connection to host failed even after configuring port forwarding";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 12: DESCRIPTION : Verify the HTTP connection is successful when portforwarding is configured in router mode");
			LOGGER.info(
					"STEP 12: ACTION : Execute the command from the WAN(Jump Server):curl --connect-timeout 20 -v <WAN IPv4 ADDRESS>");
			LOGGER.info("STEP 12: EXPECTED : HTTP connection to the host should be successful");
			LOGGER.info("**********************************************************************************");
			result = BroadBandCommonUtils.verifyConnectivityFromJumpServerUsingCurl(tapEnv, device, wanIpAddress);
			status = result.isStatus();
			if (status) {
				LOGGER.info(
						"STEP 12 :ACTUAL: HTTP connection to the host is successful, Port Forwarding is working good in router mode");
			} else {
				LOGGER.error("STEP 12 :ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * step 13 : Change the security mode of wifi in 2.4Ghz band. Expected :
			 * Security mode must be changed successfully
			 * 
			 */
			testStepNumber = "s13";
			errorMessage = "Note able to set security mode as WPA_Enterprise in 2.4Ghz private wifi network";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION: Change the security mode of wifi in 2.4Ghz band to WPA2 Enterprise ");
			LOGGER.info(
					"STEP 13: ACTION: Set the value of the web pa parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled to WPA2 Enterprise");
			LOGGER.info(
					"STEP 13: EXPECTED: Security mode of the private wifi in 2.4Ghz must be changed successfully to WPA2 Enterprise");
			LOGGER.info(
					"************************************************************************************************");
			if (!DeviceModeHandler.isRPIDevice(device)) {
				try {
					status = BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE,
							BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ);
					LOGGER.info("Waiting for One minute after setting Security mode wifi parameter");
					tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				} catch (TestException exception) {
					errorMessage += errorMessage + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 13:ACTUAL: Changed the security mode in 2.4Ghz band" + status);
				} else {
					LOGGER.error("STEP 13 :ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("WPA2 Enterprise is not present in supported modes for RPI ");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * step 14 : Change the security mode of wifi in 5Ghz band. Expected : Security
			 * mode must be changed successfully
			 * 
			 */
			testStepNumber = "s14";
			errorMessage = "Note able to set security mode as WPA_Enterprise in 5Ghz private wifi network";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 14: DESCRIPTION: Change the security mode of wifi in 5Ghz band to WPA2 Enterprise using \"Device.WiFi.AccessPoint.1.Security.ModeEnabled \"");
			LOGGER.info(
					"STEP 14: ACTION: Set the value of the web pa parameter Device.WiFi.AccessPoint.10101.Security.ModeEnabled to WPA2 Enterprise");
			LOGGER.info(
					"STEP 14: EXPECTED: Security mode of the private wifi in 5Ghz must be changed successfully tp WPA2 Enterprise ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isRPIDevice(device)) {
				try {
					status = BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE,
							BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ);
					LOGGER.info("Waiting for One minute after setting Security mode wifi parameter");
					tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				} catch (TestException exception) {
					errorMessage += exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 14:ACTUAL:Changed the security mode in 5Ghz band" + status);
				} else {
					LOGGER.error("STEP 14 :ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("WPA2 Enterprise is not present in supported modes for RPI ");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * 
			 * step 15 : Enable Bridge mode using webpa Expected : Bridge mode must be
			 * enabled successfully
			 * 
			 */
			testStepNumber = "s15";
			errorMessage = "Not able to change the device to bridge mode using "
					+ BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS;
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 15: DESCRIPTION: Enable Bridge mode using webpa ");
			LOGGER.info(
					"STEP 15: ACTION: Set webpa parameter Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode to 'bridge-static'");
			LOGGER.info(
					"STEP 15: EXPECTED: Value of Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode must be set to \"bridge-static\" successfully");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandCommonUtils.setDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device);
			} catch (TestException exception) {
				LOGGER.error("Exception occurred during execution => " + exception.getMessage());
				errorMessage = errorMessage + exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 15:ACTUAL: Bridge mode is enabled using webpa" + status);
			} else {
				LOGGER.error("STEP 15 :ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			// wait after enabling Bridge mode
			LOGGER.info("Waiting for FOUR minutes after changing mode to bridge mode");
			tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);

			/**
			 * 
			 * step 16 : verify whether Bridge mode enabled using SNMP Expected : Value of
			 * the MIB 1.3.6.1.4.1.17270.50.2.3.2.1.1.32 must be "1" in bridge mode
			 * 
			 */
			testStepNumber = "s16";
			errorMessage = "Bridge mode is not enabled even after 5 minutes of changing the device to bridge mode using Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION: verify whether Bridge mode is enabled using SNMP ");
			LOGGER.info("STEP 16: ACTION: Do snmp get on SNMP MIB 1.3.6.1.4.1.17270.50.2.3.2.1.1.32");
			LOGGER.info("STEP 16: EXPECTED: Value of  the MIB must be \"1\" in bridge mode");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandSnmpUtils.performSnmpWalkAndVerify(tapEnv, device,
						BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(),
						BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex(),
						BroadBandTestConstants.STRING_VALUE_ONE);
			} catch (TestException exception) {
				errorMessage = errorMessage + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 16:ACTUAL:Successfully Verified Bridge mode using SNMP " + status);
			} else {
				LOGGER.error("STEP 16 :ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 17: Verify whether IP tables doesn’t have the port forwarding rules
			 * configured in bridge mode
			 *
			 */
			status = false;
			testStepNumber = "s17";
			errorMessage = "IP tables have  the port forwarding rules configred in bridge mode";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 17: DESCRIPTION : Verify whether IP tables doesn’t have  the port forwarding rules configred in bridge mode");
			LOGGER.info("STEP 17: ACTION : Execute the command: cat .ipt | grep -i  <LAN-IPv4 ADDRESS> ");
			LOGGER.info("STEP 17: EXPECTED : port forwarding rules are not configured  in bridge mode");
			LOGGER.info("**********************************************************************************");
			status = !BroadBandCommonUtils.verifyPortForwardingRuleConfigurationInIPTables(tapEnv, device,
					ipAddressRetrievedFromClient);
			if (status) {
				LOGGER.info(
						"STEP 17 ACTUAL: IP tables doesn’t have  the port forwarding rules configred in bridge mode");
			} else {
				LOGGER.error("STEP 17: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 18: Verify Whether IP tables restore doesn’t fail when device in bridge
			 * mode
			 *
			 */
			testStepNumber = "s18";
			errorMessage = "IP tables-restore  failed message occurs when device in bridge mode";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 18 DESCRIPTION : Verify Whether IP tables restore doesn’t fail when device in bridge mode");
			LOGGER.info("STEP 18: ACTION : Execute the command as root@Docsis-Gateway:# firewall");
			LOGGER.info(
					"STEP 18: EXPECTED : The output should not contain the failure message as 'iptables-restore: line 43 failed' in bridge mode");
			LOGGER.info("**********************************************************************************");
			String response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.STRING_FIREWALL);
			status = CommonMethods.isNotNull(response) && !CommonUtils.patternSearchFromTargetString(response,
					BroadBandTestConstants.STRING_IPTABLES_RESTORE_FAILURE);
			if (status) {
				LOGGER.info("STEP 18: ACTUAL: IP tables restore doesn’t fail when device in bridge mode");
			} else {
				LOGGER.error("STEP 18: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * 
			 * step 19 : Disable Bridge mode using webpa Expected : Bridge mode must be
			 * disabled successfully
			 * 
			 */
			testStepNumber = "s19";
			errorMessage = "Failed to chnage the device to router mode from bridge mode using "
					+ BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS;
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 19: DESCRIPTION: Disable Bridge mode using webpa");
			LOGGER.info(
					"STEP 19: ACTION: Set web pa parameter Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode to value 'router'");
			LOGGER.info(
					"STEP 19: EXPECTED: Value of \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\" must be set to \"router\"");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandCommonUtils.setDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
			} catch (TestException exception) {
				LOGGER.error("caught TestException with message - " + exception.getMessage());
				errorMessage = errorMessage + exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 19:ACTUAL: Disabled Bridge using Web pa" + status);
			} else {
				LOGGER.error("STEP 19: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			// wait for device to turn into router
			LOGGER.info("waiting for Four minutes after disabling bridge mode");
			tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);

			/**
			 * 
			 * step 20 : Verify whether Bridge mode disabled using SNMP. Expected : Value of
			 * snmp mib 1.3.6.1.4.1.17270.50.2.3.2.1.1.32 must be "2" when the device in
			 * router mode
			 * 
			 */
			testStepNumber = "s20";
			errorMessage = "Bridge mode is not in disabled state even after 10 minutes of changing the device to router mode";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 20: DESCRIPTION: Verify whether Bridge mode disabled using SNMP MIB  1.3.6.1.4.1.17270.50.2.3.2.1.1.32 ");
			LOGGER.info("STEP 20: ACTION: Do snmp get on SNMP MIB 1.3.6.1.4.1.17270.50.2.3.2.1.1.32");
			LOGGER.info("STEP 20: EXPECTED: Value of snmp mib must be \"2\" when the device in router mode.");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandSnmpUtils.performSnmpWalkAndVerify(tapEnv, device,
						BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(),
						BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex(),
						BroadBandTestConstants.STRING_VALUE_TWO);
			} catch (TestException exception) {
				errorMessage = errorMessage + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 20:ACTUAL: Successfully verified Device in router mode using SNMP" + status);
			} else {
				LOGGER.error("STEP 20: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * step 21 : Verify the Private SSID names in both 2.4Ghz and 5Ghz bands..
			 * Expected : values of Device.WiFi.SSID.1.SSID,Device.WiFi.SSID.2.SSID must be
			 * same as they are set .
			 * 
			 */
			testStepNumber = "s21";
			errorMessage = "Private SSID name is changing when we change the device from router mode to bridge mode and vice versa for ";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 21: DESCRIPTION: Verify the Private SSID names in both 2.4Ghz and 5Ghz bands");
			LOGGER.info(
					"STEP 21: ACTION: Do a web pa get on parameters Device.WiFi.SSID.10001.SSID,Device.WiFi.SSID.10101.SSID");
			LOGGER.info(
					"STEP 21: EXPECTED: values of Device.WiFi.SSID.10001.SSID,Device.WiFi.SSID.10101.SSID must be \"HELLO_2.4\" and \"HELLO_5\" respectively ");
			LOGGER.info(
					"************************************************************************************************");
			try {
				statusFor2GhzParameter = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.PRIVATEWIFI_NAME_FOR_2GHZ_BAND);
				statusFor5GhzParameter = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.PRIVATEWIFI_NAME_FOR_5GHZ_BAND);
				status = statusFor2GhzParameter && statusFor5GhzParameter;
			} catch (TestException exception) {
				errorMessage = errorMessage + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 21:ACTUAL: Successfully verified SSID names after turning device to router mode"
						+ status);
			} else {
				LOGGER.error("STEP 21: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * step 22 : Verify the channel number in 2.4Ghz band Expected : channel number
			 * must be same as it is set in step 2.
			 * 
			 */
			testStepNumber = "s22";
			errorMessage = "Wifi channel number in 2Ghz did not remain intact after Bridge to router mode transition";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 22: DESCRIPTION: Verify the channel number in 2.4Ghz band");
			LOGGER.info("STEP 22: ACTION: Do a Web pa get on parameter Device.WiFi.Radio.10000.Channel");
			LOGGER.info(
					"STEP 22: EXPECTED: Value of Device.WiFi.Radio.10000.Channel must be same as the value that is set in step2");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
						updatedWifiChannelNumberIn2GhzBand);
			} catch (TestException exception) {
				errorMessage = errorMessage + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 22:ACTUAL: verified channel number in 2.4Ghz after returning to router mode " + status);
			} else {
				LOGGER.error("STEP 22: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * step 23 : Verify the channel number in 2.4Ghz band Expected : channel number
			 * must be same as it is set in step 2.
			 * 
			 */
			testStepNumber = "s23";
			errorMessage = "wifi channel number in 5Ghz did not remain intact after Bridge to router mode transition";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 23:DESCRITPION: Verify the channel number in 5Ghz band");
			LOGGER.info("STEP 23: ACTION: Do a Web pa get on parameter Device.WiFi.Radio.10100.Channel");
			LOGGER.info(
					"STEP 23: EXPECTED: Value of the Device.WiFi.Radio.10100.Channel must be same as the value that is set in step3");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
						updatedWifiChannelNumberIn5GhzBand);

			} catch (TestException exception) {
				errorMessage = errorMessage + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 23:ACTUAL:Verfied channel number in 5Ghz band" + status);
			} else {
				LOGGER.error("STEP 23: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * step 24 : Verify the values of remote access statuses Expected : Values must
			 * be same as the values that are set in step4
			 * 
			 */
			testStepNumber = "s24";
			errorMessage = "Remote access statuses did not remain intact after transition from Bridge to router mode transitions";
			status = false;
			statusFor2GhzParameter = false;
			statusFor5GhzParameter = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 24: DESCRIPTION: Verify the values of remote access status for both http and https ");
			LOGGER.info(
					"STEP 24: ACTION: Do a webpa get on paramters Device.UserInterface.X_CISCO_COM_RemoteAccess.HttpEnable ,Device.UserInterface.X_CISCO_COM_RemoteAccess.HttpsEnable");
			LOGGER.info("STEP 24: EXPECTED: Values must be same as the values that are set in step4");
			LOGGER.info(
					"************************************************************************************************");
			try {
				statusFor2GhzParameter = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_HTTP_REMOTE_ACCESSSTATUS,
						updatedValueofhttpRemoteAccess);
				statusFor5GhzParameter = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_HTTPS_REMOTE_ACCESSSTATUS,
						updatedValueofhttpsRemoteAccess);
				status = statusFor2GhzParameter && statusFor5GhzParameter;
			} catch (TestException exception) {
				errorMessage += errorMessage;
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 24:ACTUAL:Successfully verified the values of remote access statuses " + status);
			} else {
				LOGGER.error("STEP 24: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * step 25 : Verify the security mode of private wifi in 2.4Ghz band. Expected :
			 * Values must be same as the values that are set in step5
			 * 
			 */
			testStepNumber = "s25";
			errorMessage = "Security mode for private wifi in 2.4Ghz band did not remain intact on bridge to router transistors";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 25: DESCRIPTION: Verify the security mode of private wifi in 2.4Ghz band");
			LOGGER.info(
					"STEP 25: ACTION: Do a webpa get on parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled");
			LOGGER.info("STEP 25: EXPECTED: Value must be same as it is set in step5  ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isRPIDevice(device)) {
				try {
					status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE);
				} catch (TestException exception) {
					errorMessage = errorMessage + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 25:ACTUAL:Verified the value of  security mode in 2.4Ghz band" + status);
				} else {
					LOGGER.error("STEP 25: ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("WPA2 Enterprise is not present in supported modes for RPI ");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * step 26 : Verify the security mode of private wifi in 5Ghz band. Expected :
			 * Values must be same as the values that are set in step6
			 * 
			 */
			testStepNumber = "s26";
			errorMessage = "Security mode for private wifi in 5Ghz band did not remain intact on bridge to router transistors";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info(
					"STEP 26: DESCRIPTION: Verify the security mode of private wifi in 5Ghz band using  Device.WiFi.AccessPoint.10101.Security.ModeEnabled ");
			LOGGER.info(
					"STEP 26: ACTION: Do a webpa get on parameter Device.WiFi.AccessPoint.10101.Security.ModeEnabled");
			LOGGER.info("STEP 26: EXPECTED: Value must be same as it is set in step6 ");
			LOGGER.info(
					"************************************************************************************************");
			if (DeviceModeHandler.isRPIDevice(device)) {
				try {
					status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE);
				} catch (TestException exception) {
					errorMessage = errorMessage + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 26:ACTUAL:Verified the value of the securtiy mode in 5Ghz band " + status);
				} else {
					LOGGER.error("STEP 26: ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("WPA2 Enterprise is not present in supported modes for RPI ");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * step 27 : Verify the persistence of parental control rule Expected : Values
			 * must be same as the values that are set in step7
			 * 
			 */
			testStepNumber = "s27";
			errorMessage = "Parental control rule did not remain intact on bridge to router transitions";
			status = false;
			LOGGER.info(
					"************************************************************************************************");
			LOGGER.info("STEP 27: DESCRIPTION: Verify the persistence of parental control rule");
			LOGGER.info("STEP 27: ACTION: Do a webpa get on parameter returned in the step 7");
			LOGGER.info("STEP 27: EXPECTED: Value must be same as the value that are set in step7 ");
			LOGGER.info(
					"************************************************************************************************");
			try {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						newParameterCreatedOnSettingParentalControlRule
								+ BroadBandTestConstants.PARENTAL_CONTROL_PARAM_ARGUMENT_SITE,
						BroadBandTestConstants.URL_WIKIPEDIA);
			} catch (TestException exception) {
				LOGGER.error("caught TestException with message - " + exception.getMessage());
				errorMessage = errorMessage + exception.getMessage();
			}
			if (status) {
				LOGGER.info("STEP 27:ACTUAL:Parental control rule is persistent after Bridge to router transition "
						+ status);
			} else {
				LOGGER.error("STEP 27: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 28: Verify whether IP tables has the port forwarding rules configured
			 * when device in Router Mode
			 *
			 */
			status = false;
			testStepNumber = "s28";
			errorMessage = "IP tables doesn't have  the port forwarding rules configred in Router mode";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 28: DESCRIPTION : Verify whether IP tables has the port forwarding rules configured when device in Router Mode");
			LOGGER.info("STEP 28: ACTION : Execute the command: cat .ipt | grep -i  <LAN-IPv4 ADDRESS> ");
			LOGGER.info("STEP 28: EXPECTED : Should get the  configured portfowarding rule in router mode");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.verifyPortForwardingRuleConfigurationInIPTables(tapEnv, device,
					ipAddressRetrievedFromClient);
			if (status) {
				LOGGER.info(
						"STEP 28 ACTUAL: IP tables has the port forwarding rules configured when device in Router Mode");
			} else {
				LOGGER.error("STEP 28: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 29: Verify the port forwarding rule for HTTP service can be deleted
			 * successfully
			 */
			testStepNumber = "s29";
			errorMessage = "Unable to delete the Port Forwarding rule configured for HTTP Service.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 29: DESCRIPTION : Verify the port forwarding rule for HTTP service can be deleted successfully");
			LOGGER.info("STEP 29: ACTION : Execute the WEbpa command to delete the port forwarding rule");
			LOGGER.info("STEP 29: EXPECTED : Port Forwarding rule for HTTP service should be deleted successfully");
			LOGGER.info("**********************************************************************************");
			WebPaServerResponse deleteResponse = tapEnv.deleteTableRowUsingRestApi(device,
					portForwardingTableAddRowResponse);
			status = CommonMethods.isNotNull(deleteResponse.getMessage())
					&& deleteResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			isRuleDeleted = status;
			if (status) {
				LOGGER.info("STEP 29 :ACTUAL:  Port Forwarding rule for HTTP service is deleted successfully");
			} else {
				LOGGER.error("STEP 29 :ACTUAL: " + errorMessage + " " + deleteResponse.getMessage());
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Step 30: Verify the port forwarding feature can be disabled using WebPA
			 */
			testStepNumber = "s30";
			errorMessage = "Port Forwarding feature cannot be disabled using WebPA";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 30: DESCRIPTION : Verify the port forwarding feature can be disabled using WebPA");
			LOGGER.info(
					"STEP 30: ACTION: Execute wepa command by setting the webpa param -Device.NAT.X_Comcast_com_EnablePortMapping as false");
			LOGGER.info("STEP 30: EXPECTED: Port Forwarding feature should be disabled using WebPA");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_AND_DISABLE_PORT_FORWARDING,
					WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP 30 :ACTUAL: Port Forwarding feature is disabled successfully using WebPA");
			} else {
				LOGGER.error("STEP 30 :ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = "Exception occurred during execution => " + exception.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("1.Change the Device back to router mode if it is in Bridge mode");
			if (BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device)) {
				BroadBandPostConditionUtils.executePostConditionToDisableBirdgeMode(device, tapEnv,
						BroadBandTestConstants.CONSTANT_1);
				LOGGER.info("Waiting For Three minutes after Changing Device to router");
				tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
			}
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 2 : DESCRIPTION : Change the HTTP remote access statuses since we have altered them in STEP 4");
			LOGGER.info("POST-CONDITION 2 : ACTION : EXECUTE WEBPA COMMAND: TO CHANGE THE HTTP REMOTE ACCESS STATUS");
			LOGGER.info("POST-CONDITION 2 : EXPECTED : HTTP REMOTE ACCESS STATUS MUST BE CHANGED");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO CHANGE HTTP REMOTE ACCESS STATUS";
			status = CommonMethods
					.isNotNull(BroadBandWebPaUtils.toggleBooleanWebpaParameterValue(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_HTTP_REMOTE_ACCESSSTATUS, tapEnv))
					&& CommonMethods.isNotNull(BroadBandWebPaUtils.toggleBooleanWebpaParameterValue(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_HTTPS_REMOTE_ACCESSSTATUS, tapEnv));
			if (status) {
				LOGGER.info("POST-CONDITION 2 : ACTUAL : SUCCESSFULLY CHANGED THE HTTP REMOTE ACCESS STATUS");
			} else {
				LOGGER.error("POST-CONDITION 2 : ACTUAL : " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 3 : DESCRIPTION : Change the channel number in 2Ghz and 5Ghz bands to initial values");
			LOGGER.info(
					"POST-CONDITION 3 : ACTION : EXECUTE WEBPA COMMAND: Change the channel number in 2Ghz and 5Ghz bands to initial values");
			LOGGER.info(
					"POST-CONDITION 3 : EXPECTED : channel number in 2Ghz and 5Ghz bands MUST BE CHANGED to initial values");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO channel number in 2Ghz and 5Ghz bands";
			parameterValueMap.clear();
			parameterValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
					currentWifiChannelNumberIn2Ghz);
			parameterValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
					currentWifiChannelNumberIn5Ghz);
			status = BroadBandWebPaUtils.executeMultipleWebpaParametersSet(device, tapEnv, parameterValueMap,
					WebPaDataTypes.INTEGER.getValue());
			if (status) {
				LOGGER.info(
						"POST-CONDITION 3 : ACTUAL : SUCCESSFULLY CHANGED the channel number in 2Ghz and 5Ghz bands");
			} else {
				LOGGER.error("POST-CONDITION 3 : ACTUAL : " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 4 : DESCRIPTION : Enable auto channel Enable status in both 2.4Ghz and 5Ghz bands");
			LOGGER.info(
					"POST-CONDITION 4 : ACTION : EXECUTE WEBPA COMMAND: To Enable auto channel Enable status in both 2.4Ghz and 5Ghz bands");
			LOGGER.info(
					"POST-CONDITION 4 : EXPECTED : auto channel Enable status in both 2.4Ghz and 5Ghz bands must be enabled");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO enable auto channel Enable status in both 2.4Ghz and 5Ghz bands";
			parameterValueMap.clear();
			parameterValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
					BroadBandTestConstants.TRUE);
			parameterValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
					BroadBandTestConstants.TRUE);
			status = BroadBandWebPaUtils.executeMultipleWebpaParametersSet(device, tapEnv, parameterValueMap,
					WebPaDataTypes.BOOLEAN.getValue());
			LOGGER.info("Waiting For one Minute after setting Wifi related parameters");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"POST-CONDITION 4 : ACTUAL : SUCCESSFULLY Enabled auto channel Enable status in both 2.4Ghz and 5Ghz bands");
			} else {
				LOGGER.error("POST-CONDITION 4 : ACTUAL : " + errorMessage);
			}

			boolean setSecurityModeIn2_4Ghz = false;
			boolean setSecurityModeIn5Ghz = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 5 : DESCRIPTION : Change the security mode in both 2.4Ghz and 5Ghz bands since we have updated them in STEP5 and STEP6");
			LOGGER.info(
					"POST-CONDITION 5 : ACTION : EXECUTE WEBPA COMMAND: To Change the security mode in both 2.4Ghz and 5Ghz bands");
			LOGGER.info("POST-CONDITION 5 : EXPECTED : security mode in both 2.4Ghz and 5Ghz bands is changed");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO change security mode in both 2.4Ghz and 5Ghz bands";
			setSecurityModeIn2_4Ghz = BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,
					BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL,
					BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			LOGGER.info("Waiting for One minute after setting Security mode wifi parameter in 2Ghz band");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			setSecurityModeIn5Ghz = BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,
					BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL,
					BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			LOGGER.info("Waiting for One minute after setting Security mode wifi parameter in 5Ghz band");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = setSecurityModeIn2_4Ghz && setSecurityModeIn5Ghz;
			if (status) {
				LOGGER.info(
						"POST-CONDITION 5 : ACTUAL : SUCCESSFULLY Changed the security mode in both 2.4Ghz and 5Ghz bands since we have updated them in STEP5 and STEP6");
			} else {
				LOGGER.error("POST-CONDITION 5 : ACTUAL : " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 6 : DESCRIPTION : Delete the new parental control rule created");
			LOGGER.info(
					"POST-CONDITION 6 : ACTION : EXECUTE WEBPA COMMAND: To Delete the new parental control rule created");
			LOGGER.info(
					"POST-CONDITION 6 : EXPECTED : auto channel Enable status in both 2.4Ghz and 5Ghz bands must be disabled");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO Delete the new parental control rule created";
			WebPaServerResponse deleteParentalResponse = tapEnv.deleteTableRowUsingRestApi(device,
					newParameterCreatedOnSettingParentalControlRule);
			if (deleteParentalResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT)) {
				LOGGER.info("POST-CONDITION 6: Port Forwarding rule for HTTP service is deleted successfully");
			} else {
				LOGGER.error("POST-CONDITION 6 : ACTUAL : " + errorMessage);
			}

			if (!isRuleDeleted) {
				LOGGER.info("#####################################################################################");
				LOGGER.info(
						"POST-CONDITION 7: DESCRIPTION : Verify the port forwarding rule for HTTP service can be deleted successfully");
				LOGGER.info(
						"POST-CONDITION 7 : ACTION : EXECUTE WEBPA COMMAND: To Delete the Port Forwarding rule for HTTP service");
				LOGGER.info(
						"POST-CONDITION 7: EXPECTED : Port Forwarding rule for HTTP service should be deleted successfully");
				LOGGER.info("#######################################################################################");
				errorMessage = "Unable to delete the Port Forwarding rule configured for HTTP Service";
				WebPaServerResponse deleteResponse = tapEnv.deleteTableRowUsingRestApi(device,
						portForwardingTableAddRowResponse);
				if (deleteResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT)) {
					LOGGER.info("POST-CONDITION 7: Port Forwarding rule for HTTP service is deleted successfully");
				} else {
					LOGGER.error(
							"POST-CONDITION 7: Unable to delete the Port Forwarding rule configured for HTTP Service");
				}
			}
			LOGGER.info("################### ENDING POST-CONFIGURATIONS #########################");
			LOGGER.info("################### ENDING TEST CASE : TC-RDKB-BRIDGE-MODE-1002 #########################");
		}
	}

	/**
	 * This test case is to Verify the DHCP Lan client
	 * 
	 * 
	 * <ol>
	 * <li>STEP 1: Verify the client connected to ethernet has IP Address assigned
	 * from DHCP.</li>
	 * 
	 * <li>STEP 2: Verify whether interface got the correct IPv6 address.</li>
	 * 
	 * <li>STEP 3 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * 
	 * <li>STEP 4 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * 
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor Said Hisham
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-DHCP-LANCLIENT-1001 ")
	public void testToVerifyDhcpLanClient(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-DHCP-LANCLIENT-101";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		Dut connectedClientSettop = null;
		BroadBandResultObject result = null; // stores test result and error
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-DHCP-LANCLIENT-1001");
			LOGGER.info("TEST DESCRIPTION:Verify whether the connected client to the ethernet got the IP address");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1: Verify the client connected to ethernet has IP Address assigned from
			 * DHCP
			 *
			 */
			testStepNumber = "s1";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify the client connected to ethernet has IP Address assigned from DHCP");
			LOGGER.info(
					"STEP 1: ACTION : Client connected to ethernet from gateway should receive valid IP Address in DHCP range");
			LOGGER.info("STEP 1: EXPECTED: Client connected to LAN should be assigned with IP address from gateway");
			LOGGER.info("#####################################################################################");
			connectedClientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			errorMessage = "Unable to connect to Ethernet client";
			if (null != connectedClientSettop) {
				if (!DeviceModeHandler.isRPIDevice(device)) {
					status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv,
							device, connectedClientSettop);
				} else {
					LOGGER.info("device setup issue ... ");
					status = true;
				}
				errorMessage = "Client connected to ethernet haven't receieve valid IP Address from Gateway";
			}
			if (status) {
				LOGGER.info(
						"S1 ACTUAL: Client connected to ethernet from gateway has received valid IP Address in DHCP range");
			} else {
				LOGGER.error("S1 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2:Verify whether interface got the correct IPv6 address.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 2: DESCRIPTION : Verify whether interface  got the correct IPv6  address.");
			LOGGER.info("STEP 2: ACTION : Connected client should get the IPV6 Interface");
			LOGGER.info("STEP 2: EXPECTED:Interface IPv6 address should  be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s2";
			status = false;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				errorMessage = "interface  didnt got the correct IPV6 address";
				result = BroadBandConnectedClientUtils
						.verifyIpv6AddressOfEthInterfaceConnectedWithBroadbandDevice(connectedClientSettop, tapEnv);
				status = result.isStatus();
				errorMessage = result.getErrorMessage();
				if (status) {
					LOGGER.info("S2 ACTUAL :Interface  got the correct IPv6 address");
				} else {
					LOGGER.error("S2 ACTUAL: " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : skipping teststep ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 3:Verify whether you have connectivity using that particular interface
			 * using IPV4.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 3: Verify whether there is  connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 3: ACTION : connectivity for Ipv4 interface should be successful");
			LOGGER.info("STEP 3: EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s3";
			status = false;
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					connectedClientSettop, BroadBandTestConstants.URL_W3SCHOOLS, BroadBandTestConstants.IP_VERSION4);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S3 ACTUAL: connectivity successful using ipv4 interface");
			} else {
				LOGGER.error("S3 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 4:Verify whether there is connectivity using that particular interface
			 * using IPV6.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 4: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("STEP 4: ACTION : connectivity for Ipv6 interface should be successful");
			LOGGER.info("STEP 4:EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			status = false;
			testStepNumber = "s4";
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
						connectedClientSettop, BroadBandTestConstants.URL_HTTPS_FACEBOOK,
						BroadBandTestConstants.IP_VERSION6);
				status = result.isStatus();
				errorMessage = result.getErrorMessage();
				if (status) {
					LOGGER.info("S4 ACTUAL: connectivity successful using ipv6 interface");
				} else {
					LOGGER.error("S4 ACTUAL: " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : skipping teststep ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, true);
		}
		LOGGER.info("ENDING TESTCASE :TC-RDKB-DHCP-LANCLIENT-1001");
	}

}
