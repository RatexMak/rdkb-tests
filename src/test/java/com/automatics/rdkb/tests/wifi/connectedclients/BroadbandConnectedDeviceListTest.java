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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
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
}
