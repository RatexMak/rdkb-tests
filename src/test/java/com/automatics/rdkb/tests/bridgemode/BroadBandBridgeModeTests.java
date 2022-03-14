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
package com.automatics.rdkb.tests.bridgemode;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandBridgeModeUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.moca.MocaUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Class to hold the test script for Bridge mode test scenarios
 * 
 * @author revanth.k
 */

public class BroadBandBridgeModeTests extends AutomaticsTestBase {

	/**
	 * Verification of enabling and disabling Bridge mode via SNMP
	 * <li>PRE CONDITION 1: Verify whether the private SSID's ,Public WiFi SSID's
	 * are enabled by default</li>
	 * <li>PRE CONDITION 2: Check whether all the required wifi parameters are
	 * enabled</li>
	 * <li>PRE CONDITION 3: Enable Moca.</li>
	 * <li>STEP 1: Enable bridge mode via snmp mib value must be successfully set to
	 * 1</li>
	 * <li>STEP 2: Verify whether bridge mode is enabled using webpa value of webpa
	 * parameter "device.x_cisco_com_devicecontrol.lanmanagemententry.1.lanmode"
	 * must be "bridge-static".</li>
	 * <li>STEP 3: Verify the private ssid's in both 2.4ghz and 5ghz bands. private
	 * ssid's in both the bands should be disabled.</li>
	 * <li>STEP 4: Verify the public wifi ssid's in both 2.4ghz and 5ghz
	 * bands.Public wifi ssid's should not be disabled in bridge mode</li>
	 * <li>STEP 5: Verify the moca status in bridge mode moca should be disabled in
	 * bridge mode</li>
	 * <li>STEP 6: Verify whether the bridge mode persists on reboot. device must be
	 * in bridge mode even after reboot and the value of
	 * "device.x_cisco_com_devicecontrol.lanmanagemententry.1.lanmode" must be
	 * "bridge-static"</li>
	 * <li>STEP 7:Disable bridge mode using snmp mib value must be set to 2
	 * successfully and the device should return back to router mode.</li>
	 * <li>STEP 8:Check whether bridge mode is disabled using webpa</li>.
	 * <li>device must return back to router mode. value of
	 * device.x_cisco_com_devicecontrol.lanmanagemententry.1.lanmode must be
	 * "router".</li>
	 * <li>STEP 9: Check the private ssid's in both 2.4ghz and 5ghz channel private
	 * ssid's in both bands should be enabled in router mode.</li>
	 * <li>STEP 10: Check the moca status in router mode moca should be enabled in
	 * router mode.</li>
	 * <li>POST-CONDITION 1: Set the device back to router mode if device is in
	 * Bridge mode</li>
	 * </ol>
	 * 
	 * @param device
	 * @author Divya R S, Gnanaprakasham S
	 * @Refactor Sruthi Santhosh
	 **/

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-BRIDGE-MODE-1001")
	public void testToEnableDisableBridgModeUsingSnmp(Dut device) {

		// Variable for test case ID
		String testCaseId = "TC-RDKB-BRIDGE-MODE-001";
		// Variable for holding execution status
		boolean status = false;
		String errorMessage = null;
		String stepNumber = "s1";
		boolean validatePrivateSsid2GhzStatus = false;
		boolean validatePrivateSsid5GhzStatus = false;
		boolean publicWifi2GhzStatus = false;
		boolean publicWifi5GhzStatus = false;
		long startTime = 0L;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE : TC-RDKB-BRIDGE-MODE-1001");
			LOGGER.info(
					"TEST DESCRIPTION: Test to verify SSID's enabled/disabled status when bridge mode enabled/disable using SNMP");
			LOGGER.info(
					"PRE CONDITION 1: The private SSID's, Public wifi ssid's for both 2.4Ghz and 5Ghz must be  enabled.");
			LOGGER.info("PRE CONDITION 2: check whether all the required wifi parameters are enabled");
			LOGGER.info("PRE CONDITION 3: Enable Moca.");
			LOGGER.info("STEP 1: Enable bridge mode via snmp mib value must be successfully set to 1");
			LOGGER.info(
					"STEP 2: Verify whether bridge mode is enabled using webpa value of webpa parameter device.x_cisco_com_devicecontrol.lanmanagemententry.1.lanmode' must be 'bridge-static'");
			LOGGER.info(
					"STEP 3: Verify the private ssid's in both 2.4ghz and 5ghz bands. private ssid's in both the bands should be disabled");
			LOGGER.info(
					"STEP 4: Verify the public wifi ssid 5ghz band. Public wifi ssid should not be disabled in bridge mode");
			LOGGER.info("STEP 5: Verify the moca status in bridge mode <li>moca should be disabled in bridge mode");
			LOGGER.info(
					"STEP 6: Verify whether the bridge mode persists on reboot. device must be in bridge mode even after reboot and the value of 'device.x_cisco_com_devicecontrol.lanmanagemententry.1.lanmode' must be 'bridge-static'");
			LOGGER.info(
					"STEP 7:Disable bridge mode using snmp mib value must be set to 2 successfully and the device should return back to router mode.");
			LOGGER.info(
					"STEP 8:Check whether bridge mode is disabled using webpa . device must return back to router mode. value of device.x_cisco_com_devicecontrol.lanmanagemententry.1.lanmode must be 'router'.");
			LOGGER.info(
					"STEP 9: Check the private ssid's in both 2.4ghz and 5ghz channel private ssid's in both bands should be enabled in router mode.");
			LOGGER.info("STEP 10: Check the moca status in router mode moca should be enabled in router mode.");
			LOGGER.info("POST-CONDITION 1: Set the device back to router mode if device is in Bridge mode");
			LOGGER.info("#######################################################################################");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 1 : DESCRIPTION: Verify whether the private SSID's,Public WiFi SSID's are enabled by default.");
			LOGGER.info("PRE-CONDITION 1 : ACTION: Execute WebPA command to enable private SSID's,Public WiFi SSID");
			LOGGER.info("PRE-CONDITION 1 : EXPECTED: WebPA request should return success message");
			LOGGER.info("#######################################################################################");
			BroadBandBridgeModeUtils.preConditionsForBridgeModeTestCases(device, tapEnv);

			// check whether all the required wifi parameters are enabled
			errorMessage = "As a pre condition failed to enable private SSID's for both 2.4 and 5 GHz";
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 2 : DESCRIPTION: check whether all the required wifi parameters are enabled");
			LOGGER.info(
					"PRE-CONDITION 2 : ACTION: Execute WebPA command to verify the required wifi parameters are enabled");
			LOGGER.info("PRE-CONDITION 2 : EXPECTED: WebPA request should return success message");
			LOGGER.info("#######################################################################################");
			BroadBandWiFiUtils.verifyWiFiParameterStatus(tapEnv, device, true);

			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 3 : DESCRIPTION: Enable MOCA using WebpA");
			LOGGER.info("PRE-CONDITION 3 : ACTION: Execute WebPA command to enable MOCA");
			LOGGER.info("PRE-CONDITION 3 : EXPECTED: WebPA request should return success message");
			LOGGER.info("#######################################################################################");
			errorMessage = "failed to enable moca As a pre condition";

			if (DeviceModeHandler.isBusinessClassDevice(device)) {
				LOGGER.info(
						"Obtained device is a Business class device.Enable Moca is Not applicable For those devices.");
			} else {
				MocaUtils.enableMocaAndVerifyTheEnableStatus(tapEnv, device);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : Enable bridge mode using SNMP mib
			 */
			errorMessage = "Not able to enable bridge mode using SNMP MIB 1.3.6.1.4.1.17270.50.2.3.2.1.1";
			LOGGER.info(
					"*********************************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION:Enable Bridge mode via SNMP");
			LOGGER.info("STEP 1: ACTION:Set SNMP mib 1.3.6.1.4.1.17270.50.2.3.2.1.1.32 to 1");
			LOGGER.info("STEP 1: EXPECTED: MIB value must be successfully set to 1 ");
			LOGGER.info(
					"*********************************************************************************************");
			startTime = System.currentTimeMillis();
			do {
				try {
					status = BroadBandBridgeModeUtils.enableDisableBridgeModeThroughSnmp(tapEnv, device,
							BroadBandTestConstants.STRING_VALUE_ONE);
				} catch (TestException exception) {
					errorMessage = exception.getMessage();
					LOGGER.error("TestException occurred during execution =>" + exception.getMessage());
				}
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.ONE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TEN_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 1:ACTUAL:Enable Bridge Mode through snmp -" + status);
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 2 : Verify Whether Bridge mode is enabled using webpa command
			 */
			stepNumber = "s2";
			errorMessage = "Bridge mode is not enabled even after 3 minutes of changing the router to bridge mode using snmp mib";
			status = false;
			LOGGER.info(
					"*********************************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION:Verify Whether Bridge mode is enabled using webpa \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\" ");
			LOGGER.info(
					"STEP 2: ACTION: Do a Webpa get on parameter Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode");
			LOGGER.info(
					"STEP 2: EXPECTED: Value of the webpa parameter \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\" must be \"bridge-static\"");
			LOGGER.info(
					"*********************************************************************************************");
			startTime = System.currentTimeMillis();
			do {
				try {
					status = BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device);
				} catch (TestException exception) {
					LOGGER.error("TestException caught with message :" + exception.getMessage() + " : Retrying");
				}
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Bridge mode enabled status : " + status);
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 3 : Verify the Private SSID's in both 2.4Ghz and 5GHz bands
			 */
			stepNumber = "s3";
			errorMessage = "Private SSID's are not disabled in bridge mode";
			status = false;
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Verify the Private SSID's in both 2.4Ghz and 5GHz bands using  Device.WiFi.SSID.{i}.Enable");
			LOGGER.info(
					"STEP 3: ACTION: Do a webpa get on paramaters Device.WiFi.SSID.10001.Enable , Device.WiFi.SSID.10101.Enable");
			LOGGER.info("STEP 3: EXPECTED: Private SSID's in both the Bands should be disabled.");
			LOGGER.info(
					"************************************************************************************************************************");
			try {
				validatePrivateSsid2GhzStatus = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE,
						BroadBandTestConstants.FALSE, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				LOGGER.info("Private SSID enabled status for 2.4GHz via WebPa: " + validatePrivateSsid2GhzStatus);
				if (!validatePrivateSsid2GhzStatus) {
					validatePrivateSsid2GhzStatus = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv,
							device, BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getOid(),
							BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getTableIndex(),
							BroadBandTestConstants.STRING_VALUE_TWO);
					LOGGER.info("Private SSID enabled status for 2.4GHz via SNMP: " + validatePrivateSsid2GhzStatus);
					if (!validatePrivateSsid2GhzStatus) {
						LOGGER.error(
								"Private SSID for 2.4 GHZ is not in disabled state in bridge mode. Expected value should be false. But actual value :"
										+ validatePrivateSsid2GhzStatus);
						errorMessage += " 2.4GHz ,";
					}
				}
			} catch (TestException exception) {
				errorMessage += exception.getMessage();
				LOGGER.error(errorMessage);
			}
			try {
				validatePrivateSsid5GhzStatus = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						BroadBandTestConstants.FALSE, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				LOGGER.info("Private SSID enabled status for 5GHz via WebPa: " + validatePrivateSsid5GhzStatus);
				if (!validatePrivateSsid5GhzStatus) {
					validatePrivateSsid5GhzStatus = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv,
							device, BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(),
							BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex(),
							BroadBandTestConstants.STRING_VALUE_TWO);
					LOGGER.info("Private SSID enabled status for 5GHz via SNMP: " + validatePrivateSsid5GhzStatus);
					if (!validatePrivateSsid5GhzStatus) {
						LOGGER.error(
								"Private SSID is not in disabled state for 5 GHZ in bridge mode. Expected value should be false. But actual value :"
										+ validatePrivateSsid5GhzStatus);
						errorMessage += " 5GHz";
					}
				}
			} catch (TestException exception) {
				errorMessage += exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (validatePrivateSsid5GhzStatus && validatePrivateSsid2GhzStatus);
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Enable status for Device.WiFi.SSID.10001.Enable,Device.WiFi.SSID.10101.Enable "
								+ status);
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 4 :Verify the Public wifi SSID in 5Ghz band.
			 */
			stepNumber = "s4";
			errorMessage = "Public WiFi SSID is not enabled in bridge mode for ";
			status = false;
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION:Verify the Public wifi SSID in 5Ghz band ");
			LOGGER.info("STEP 4: ACTION:Do a webpa get on paramater Device.WiFi.SSID.100103.Enable");
			LOGGER.info("STEP 4: EXPECTED: Public wifi SSID should not be disabled in Bridge mode ");
			LOGGER.info(
					"************************************************************************************************************************");
			try {
				publicWifi5GhzStatus = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS,
						BroadBandTestConstants.TRUE, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				publicWifi5GhzStatus = BroadBandWiFiUtils.validateStatusUsingWebPa(tapEnv, device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS, true);
				LOGGER.info("Public WiFi SSID enabled status for 5 GHz via Webpa: " + publicWifi5GhzStatus);
				if (!publicWifi5GhzStatus) {
					publicWifi5GhzStatus = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv, device,
							BroadBandSnmpMib.PUBLIC_5_SSID_STATUS.getOid(),
							BroadBandSnmpMib.PUBLIC_5_SSID_STATUS.getTableIndex(),
							BroadBandTestConstants.STRING_VALUE_TWO);
					LOGGER.info("Public WiFi SSID enabled status for 5 GHz via SNMP: " + publicWifi5GhzStatus);
					if (!publicWifi5GhzStatus) {
						LOGGER.error(
								"Public WiFi SSID'for 5 GHZ is not in enabled state after enabling router mode. Expected value should be true. But actual value :"
										+ publicWifi5GhzStatus);

						errorMessage += " 5 GHz ,";
					}
				}
			} catch (TestException exception) {
				errorMessage += exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (publicWifi5GhzStatus);
			if (status) {
				LOGGER.info("STEP 4:ACTUAL:Enable status of Public Wifi in 5 Ghz -" + status);
			} else {
				LOGGER.error(errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 5 :Verify the MOCA status in Bridge mode
			 */
			stepNumber = "s5";
			errorMessage = "Moca is not in disabled state in bridge mode ";
			status = false;
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Verify the MOCA status in Bridge mode");
			LOGGER.info("STEP 5: ACTION: Do a webpa get on parameter Device.MoCA.Interface.1.Enable");
			LOGGER.info("STEP 5: EXPECTED: MOCA should be disabled in Bridge mode ");
			LOGGER.info(
					"************************************************************************************************************************");
			if (DeviceModeHandler.isBusinessClassDevice(device)) {
				errorMessage = "Moca related steps are not applicable for Business class devices";
				LOGGER.info(errorMessage);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			} else {
				try {
					MocaUtils.validateMocaStatus(tapEnv, device, false);
					status = true;
					LOGGER.info("STEP 5:ACTUAL:MOCA is disabled in Bridge Mode :" + status);
				} catch (TestException exception) {
					status = false;
					errorMessage += exception.getMessage();
				}
				if (status) {
					LOGGER.info("STEP 5: ACTUAL : MOCA is disabled in Bridge Mode :" + status);
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			}

			/**
			 * Step 6 :Verify whether the Bridge mode persists on reboot.
			 */
			stepNumber = "s6";
			errorMessage = "Failed to verify the bridge mode status after reboot";
			status = false;
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION: Verify whether the Bridge mode persists on reboot ");
			LOGGER.info(
					"STEP 6: ACTION: Do a reboot and check the Bridge mode status using Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode");
			LOGGER.info(
					"STEP 6: EXPECTED: Device must be in bridge mode even after reboot and the value  of \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode\" must be \"bridge-static\" ");
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info("Going to reboot and wait till IP acquisition");
			if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
				// Verify the bridge mode.
				errorMessage = "Failed to verify that the lan mode of the device as bridge-static after reboot. ";
				try {
					errorMessage = "Device found to be NOT in Bridge mode after reboot";
					status = BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device);
				} catch (TestException exception) {
					status = false;
					errorMessage += exception.getMessage();
				}
			} else {
				errorMessage = "BroadBand device doesn't come up online after reboot or doesn't get the ip for ssh connectivity";
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 6:ACTUAL: Bridge is persistent after Reboot -" + status);
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 7 :Disable Bridge mode using snmp MIB and verify
			 */
			stepNumber = "s7";
			errorMessage = "Not able to disable bridge mode using SNMP mib \"1.3.6.1.4.1.17270.50.2.3.2.1.1.32 i 2\"";
			status = false;
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info(
					"STEP 7: DESCRIPTION: Disable  Bridge mode via SNMP mib \"snmpset -v2c -r1 -t3 -c <community string> udp6:<ecm IP>:161 1.3.6.1.4.1.17270.50.2.3.2.1.1.32 i 2\" ");
			LOGGER.info("STEP 7: ACTION: set SNMP mib 1.3.6.1.4.1.17270.50.2.3.2.1.1.32 to 2");
			LOGGER.info(
					"STEP 7: EXPECTED: Value must be set to 2 successfully and the Device should retun back to router mode");
			LOGGER.info(
					"************************************************************************************************************************");
			startTime = System.currentTimeMillis();
			do {
				try {
					status = BroadBandBridgeModeUtils.enableDisableBridgeModeThroughSnmp(tapEnv, device,
							BroadBandTestConstants.STRING_VALUE_TWO);
				} catch (TestException exception) {
					errorMessage += exception.getMessage();
					LOGGER.error("TestException caught with message :" + exception.getMessage());
				}
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.ONE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TEN_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 7:ACTUAL:Bridge Mode is disabled using SNMP -" + status);
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 8 :Check whether Bridge mode is disabled using webpa.
			 */
			stepNumber = "s8";
			errorMessage = "Bridge mode is not disabled even after 3 minutes of changing the router to router mode ";
			status = false;
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info(
					"STEP 8: DESCRIPTION: Check whether Bridge mode is disabled  using webpa \"Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode \"");
			LOGGER.info(
					"STEP 8: ACTION:Do a webpa get on paramater Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode");
			LOGGER.info(
					"STEP 8: EXPECTED: Device must return  back to router mode. Value of  Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode must be \"router\".      ");
			LOGGER.info(
					"************************************************************************************************************************");
			startTime = System.currentTimeMillis();
			do {
				try {
					status = BroadBandCommonUtils.verifyDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
				} catch (TestException exception) {
					LOGGER.error("TestException caught with message :" + exception.getMessage() + " : Retrying");
				}
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 8:ACTUAL:Verified Bridge mode disable status using webpa -" + status);
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 9 :Check the Private SSID's in both 2.4Ghz and 5Ghz channel
			 */
			stepNumber = "s9";
			validatePrivateSsid2GhzStatus = false;
			validatePrivateSsid5GhzStatus = false;
			errorMessage = "Private SSID's are not in enabled state after changing the box to router mode using SNMP MIB";
			status = false;
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION: Verify the Private SSID's in both 2.4Ghz and 5GHz bands");
			LOGGER.info(
					"STEP 9: ACTION:Do a webpa get on parameters Device.WiFi.SSID.10001.Enable , Device.WiFi.SSID.10101.Enable");
			LOGGER.info("STEP 9: EXPECTED: Private SSID's in both the Bands should be enabled");
			LOGGER.info(
					"************************************************************************************************************************");
			try {
				validatePrivateSsid2GhzStatus = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE,
						BroadBandTestConstants.TRUE, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				LOGGER.info("Private SSID enabled status for 2.4GHz via Webpa: " + validatePrivateSsid2GhzStatus);
				if (!validatePrivateSsid2GhzStatus) {
					validatePrivateSsid2GhzStatus = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv,
							device, BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getOid(),
							BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getTableIndex(),
							BroadBandTestConstants.STRING_VALUE_ONE);
					LOGGER.info("Private SSID enabled status for 2.4GHz via SNMP: " + validatePrivateSsid2GhzStatus);
					if (!validatePrivateSsid2GhzStatus) {
						LOGGER.error(
								"Private SSID for 2.4 GHZ is not in enabled state after enabling router mode. Expected value should be true. But actual value :"
										+ validatePrivateSsid2GhzStatus);
						errorMessage += " 2.4GHz ,";
					}
				}
			} catch (TestException exception) {
				LOGGER.error(exception.getMessage());
			}
			try {
				validatePrivateSsid5GhzStatus = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						BroadBandTestConstants.TRUE, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				LOGGER.info("Private SSID enable status for 5GHz via Webpa: " + validatePrivateSsid5GhzStatus);
				if (!validatePrivateSsid5GhzStatus) {
					validatePrivateSsid5GhzStatus = BroadBandSnmpUtils.performSnmpGetOnRdkDevicesAndVerify(tapEnv,
							device, BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(),
							BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex(),
							BroadBandTestConstants.STRING_VALUE_ONE);
					LOGGER.info("Private SSID enable status for 5GHz via SNMP: " + validatePrivateSsid5GhzStatus);
					if (!validatePrivateSsid5GhzStatus) {
						LOGGER.error(
								"Private SSID for 5 GHZ is not in enabled state after enabling router mode. Expected value should be true. But actual value :"
										+ validatePrivateSsid5GhzStatus);
						errorMessage += " 5GHz ,";
					}
				}
			} catch (TestException exception) {
				LOGGER.error(exception.getMessage());
			}
			status = validatePrivateSsid5GhzStatus && validatePrivateSsid2GhzStatus;
			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Private SSID's are enabled in 2.4Ghz and 5Ghz -" + status);
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 10 :Check the MOCA status in router mode
			 */
			stepNumber = "s10";
			status = false;
			errorMessage = "Moca is not enabled in router mode ";
			LOGGER.info(
					"************************************************************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION: Verify the MOCA status after disabling bridge mode");
			LOGGER.info("STEP 10: ACTION: Do a webpa get on parameter Device.MoCA.Interface.1.Enable");
			LOGGER.info("STEP 10: EXPECTED: MOCA should be enabled in router mode ");
			LOGGER.info(
					"************************************************************************************************************************");
			if (DeviceModeHandler.isBusinessClassDevice(device)) {
				errorMessage = "Moca related steps are not applicable for Business class devices";
				LOGGER.info(errorMessage);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			} else {
				try {
					MocaUtils.validateMocaStatus(tapEnv, device, true);
					status = true;
				} catch (TestException exception) {
					errorMessage += exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info(
							"STEP 10: ACTUAL : MOCA is enabled after turning device back to router mode -" + status);
				} else {
					LOGGER.error("STEP 10 : ACTUAL :" + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			}
		} catch (Exception exception) {
			errorMessage = errorMessage + " Exception occurred : " + exception.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, false, errorMessage, true);
		} finally {
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 1: DESCRIPTION : Set the device back to router mode if device is in Bridge mode");
			LOGGER.info(
					"POST-CONDITION 1: ACTION : Execute the Webpa Set command for following param: Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode and set value as 'router'.");
			LOGGER.info("POST-CONDITION 1: EXPECTED : Device must be set in Router Mode.");
			LOGGER.info("#######################################################################################");
			status = false;
			errorMessage = "Unable to set the device is Router Mode";
			try {
				if (BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device)) {
					LOGGER.info("Device is found to be in Bridge mode so changing it to Router mode");
					status = BroadBandCommonUtils.setDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
					LOGGER.info("Waiting for FIVE minutes after disabling Bridge mode");
					tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTES);
				} else {
					LOGGER.info("Device is NOT in Bridge mode");
					status = true;
				}
			} catch (Exception exception) {
				LOGGER.info("Device is found to be Router mode" + exception.getMessage());
			}
			if (status) {
				LOGGER.info("POST-CONDITION 1 : ACTUAL: DEVICE IS SET TO ROUTER MODE SUCCESSFULLY.");
			} else {
				LOGGER.error("POST-CONDITION 1: ACTUAL: " + errorMessage);
			}
			LOGGER.info("##########################################################################");
			LOGGER.info("ENDING TEST CASE: " + testCaseId);
		}
	}
	
    /**
     * Test script to validate basic WiFi parameters on router mode transition
     * 
     * <ol>
     * <li>Verify dumping the Ipv4 and Ipv6 tables to local files.</li>
     * <li>Verify getting erouter0 Ipv4 and Ipv6 addresses and save for future validation.</li>
     * <li>Verify enabling Bridge mode using WebPa.</li>
     * <li>Verify whether Bridge mode is enabled via SNMP.</li>
     * <li>Verify erouter0 Ipv4 and Ipv6 addresses with step 2 results.</li>
     * <li>Verify brlan0 configuration in Bridge mode.</li>
     * <li>Verify disabling Bridge mode using WebPa.</li>
     * <li>Verify whether Bridge mode is disabled via SNMP.</li>
     * <li>Verify Ip tables after Bridge Mode to Router transition.</li>
     * <li>Verify brlan0 configuration in Router mode.</li>
     * <li>Verify setting device in LAN Mode as currently in Bridge Mode.</li>
     * <li>Verify deleting all the created files in /tmp folder.</li>
     * 
     * @param device
     * 
     * @author Revanth K, Gnanaprakasham S
     * @refactor Athira
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-BRIDGE-MODE-1003")
    public void testToVerifyRoutingFunctionalitiesAcrossModeTranisitions(Dut device) {

	// Variable declaration starts
	boolean status = false;
	String testId = "TC-RDKB-BRIDGE-MODE-003";
	String testStepNumber = "s1";
	String response = null;
	String errorMessage = "Not able to dump Ipv4 table and Ipv6 table in local files.";
	String erouter0Ipv6AddressInRouterMode = null;
	String erouter0Ipv4AddressInRouterMode = null;
	String erouter0Ipv4AddressInBridgeMode = null;
	String erouter0Ipv6AddressInBridgeMode = null;
	long startTime = 0L;
	// Variable declaration ends

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-BRIDGE-MODE-1003");
	    LOGGER.info("TEST DESCRIPTION: Test to verify routing functionalities on mode transitions.");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Verify dumping the Ipv4 and Ipv6 tables to local files.");
	    LOGGER.info("2. Verify getting erouter0 Ipv4 and Ipv6 addresses and save for future validation.");
	    LOGGER.info("3. Verify enabling Bridge mode using WebPa.");
	    LOGGER.info("4. Verify whether Bridge mode is enabled via SNMP.");
	    LOGGER.info("5. Verify erouter0 Ipv4 and Ipv6 addresses with step 2 results.");
	    LOGGER.info("6. Verify brlan0 configuration in Bridge mode.");
	    LOGGER.info("7. Verify disabling Bridge mode using WebPa.");
	    LOGGER.info("8. Verify whether Bridge mode is disabled via SNMP.");
	    LOGGER.info("9. Verify Ip tables after Bridge Mode to Router transition.");
	    LOGGER.info("10. Verify brlan0 configuration in Router mode.");
	    LOGGER.info("POST CONDITION 1. Verify setting device in LAN Mode as currently in Bridge Mode.");
	    LOGGER.info("POST CONDITION 2. Verify deleting all the created files in /tmp folder.");
	    LOGGER.info("#######################################################################################");

	    LOGGER.info("*******************************************************************************************");
	    LOGGER.info("STEP 1:DESCRITPION: Verify dumping the Ipv4 and Ipv6 tables to local files.");
	    LOGGER.info(
		    "STEP 1: ACTION: SSH the device and Execute the following commands: 'iptables-save > /tmp/ipv4tableinitial' & 'ip6tables-save > /tmp/ipv6tableinitial'.");
	    LOGGER.info(
		    "STEP 1: EXPECTED: Results of the commands must be saved saved successfully without any issue.");
	    LOGGER.info("*******************************************************************************************");
	    try {
		status = BroadBandCommonUtils.dumpIpTablesToGivenLocation(device, tapEnv,
			BroadBandTestConstants.FILE_NAME_TO_STORE_INITIAL_IPV4_TABLE_RULES,
			BroadBandTestConstants.FILE_NAME_TO_STORE_INITIAL_IPV6_TABLE_RULES);
	    } catch (Exception exception) {
		errorMessage = errorMessage
			+ " Exception occurred while dumping the Ipv4 and Ipv6 tables to local files-> "
			+ exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Dumped initial Ipv4 and Ipv6 tables to local files successfully.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s2";
	    errorMessage = "Not able to get the erouter Ipv4 and Ipv6 address from device using '/sbin/ifconfig erouter0' command.";
	    status = false;
	    LOGGER.info("*******************************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRITPION: Verify getting erouter0 Ipv4 and Ipv6 addresses and save for future validation.");
	    LOGGER.info(
		    "STEP 2: ACTION: SSH the device and Execute the following command: '/sbin/ifconfig erouter0\' and get Ipv4 & Ipv6 addresses.");
	    LOGGER.info("STEP 2: EXPECTED: erouter0 Ipv4 and Ipv6 addresses must be saved successfully.");
	    LOGGER.info("*******************************************************************************************");
	    try {
		erouter0Ipv4AddressInRouterMode = CommonMethods.getErouter0ipv4Address(device, tapEnv);
		erouter0Ipv6AddressInRouterMode = CommonMethods.getErouter0ipv6Address(device, tapEnv);
		LOGGER.info("erouter0 Ipv4 Address In Router Mode: " + erouter0Ipv4AddressInRouterMode);
		LOGGER.info("erouter0 Ipv6 Address In Router Mode: " + erouter0Ipv6AddressInRouterMode);
		status = CommonMethods.isNotNull(erouter0Ipv4AddressInRouterMode)
			&& CommonMethods.isNotNull(erouter0Ipv6AddressInRouterMode);
	    } catch (TestException exception) {
		errorMessage = errorMessage + " Exception occurred while getting erouter Ipv64 and Ipv4 addresses-> "
			+ exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully obtained erouter Ipv4 and Ipv6 addresses from the device.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s3";
	    status = false;
	    errorMessage = "Not able to change the device Lan Mode to Bridge Mode.";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP 3:DESCRITPION: Verify enabling Bridge mode using WebPa.");
	    LOGGER.info(
		    "STEP 3: ACTION: Execute the Webpa Set command for following param: Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode and set value as 'bridge-static'.");
	    LOGGER.info("STEP 3: EXPECTED: Device must be set in Bridge Mode.");
	    LOGGER.info(
		    "************************************************************************************************");

	    try {
		status = BroadBandCommonUtils.setDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device);

	    } catch (TestException exception) {
		errorMessage = errorMessage + "Exception occurred while setting device Lan Mode to Bridge Mode => "
			+ exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Device Lan Mode is changed to Bridge Mode successfully.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s4";
	    status = false;
	    errorMessage = "Bridge mode is not enabled even after 3 minutes of changing the device to bridge mode.";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION: Verify whether Bridge mode is enabled via SNMP.");
	    LOGGER.info("STEP 4: ACTION: Execute SNMP Get command for OID: 1.3.6.1.4.1.17270.50.2.3.2.1.1.32");
	    LOGGER.info("STEP 4: EXPECTED: SNMP output should be '1'.");
	    LOGGER.info(
		    "************************************************************************************************");
	    startTime = System.currentTimeMillis();
	    do {
		try {
		    status = BroadBandSnmpUtils.performSnmpWalkAndVerify(tapEnv, device,
			    BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(),
			    BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex(),
			    BroadBandTestConstants.STRING_VALUE_ONE);
		} catch (TestException exception) {
		    errorMessage = errorMessage + exception.getMessage();
		    LOGGER.error(errorMessage);
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Device Lan Mode as Bridge Mode verified successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s5";
	    status = false;
	    errorMessage = "Cross verification of erouter0 Ipv4 and Ipv6 addresses with step 2 results failed.";
	    LOGGER.info("*******************************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION: Verify erouter0 Ipv4 and Ipv6 addresses with step 2 results.");
	    LOGGER.info(
		    "STEP 5: ACTION: SSH the device and Execute the following command: '/sbin/ifconfig erouter0\' and get Ipv4 & Ipv6 addresses.");
	    LOGGER.info(
		    "STEP 5: EXPECTED: erouter0 Ipv4 and Ipv6 addresses must be same as they are obtained in step 2.");
	    LOGGER.info("*******************************************************************************************");
	    try {
		erouter0Ipv4AddressInBridgeMode = CommonMethods.getErouter0ipv4Address(device, tapEnv);
		LOGGER.info("erouter0 Ipv4 Address In Bridge Mode: " + erouter0Ipv4AddressInBridgeMode);
		erouter0Ipv6AddressInBridgeMode = CommonMethods.getErouter0ipv6Address(device, tapEnv);
		LOGGER.info("erouter0 Ipv6 Address In Bridge Mode: " + erouter0Ipv6AddressInBridgeMode);
		status = CommonMethods.patternMatcher(erouter0Ipv4AddressInBridgeMode, erouter0Ipv4AddressInRouterMode)
			&& CommonMethods.patternMatcher(erouter0Ipv6AddressInBridgeMode,
				erouter0Ipv6AddressInRouterMode);
	    } catch (Exception exception) {
		errorMessage = errorMessage + "Exception occurred while verifying erouter0 Ipv4 and Ipv6 addresses  => "
			+ exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : erouter0 Ipv4 and Ipv6 addresses are verified successfully in Bridge Mode.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s6";
	    status = false;
	    errorMessage = "brlan0 configuration has IPv4 and IPv6 address fields in Bridge Mode.";
	    LOGGER.info("*******************************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION: Verify brlan0 configuration in Bridge mode.");
	    LOGGER.info("STEP 6: ACTION: SSH the device and Execute the following command: 'ifconfig brlan0'.");
	    LOGGER.info(
		    "STEP 6: EXPECTED: There should not be any Ip addresses associated with brlan0 interface in bridge mode.");
	    LOGGER.info("*******************************************************************************************");
	    if (!DeviceModeHandler.isFibreDevice(device)) {
		try {
		    status = !BroadBandCommonUtils.validateBrlan0Configuration(device, tapEnv);
		} catch (TestException exception) {
		    errorMessage = errorMessage
			    + "Exception occured while verifying IPv4 and IPv6 address in brlan0 configuration in Bridge Mode  => "
			    + exception.getMessage();
		    LOGGER.error(errorMessage);
		}
		if (status) {
		    LOGGER.info(
			    "STEP 6: ACTUAL : erouter0 Ipv4 and Ipv6 addresses in brlan0 configuration are verified successfully in Bridge Mode.");
		} else {
		    LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    } else {
		errorMessage = "skippig brlan0 bridge mode validations as it is a Fibre device";
		LOGGER.info("STEP 6: ACTUAL : " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    testStepNumber = "s7";
	    status = false;
	    boolean isSetToRouterMode = false;
	    errorMessage = "Not able to change the device Lan Mode to Router Mode.";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION: Verify disabling Bridge mode using WebPa.");
	    LOGGER.info(
		    "STEP 7: ACTION: Execute the Webpa Set command for following param: Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode and set to value 'router'.");
	    LOGGER.info("STEP 7: EXPECTED: Device must be set to Router Mode.");
	    LOGGER.info(
		    "************************************************************************************************");
	    isSetToRouterMode = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS,
		    BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER, WebPaDataTypes.STRING.getValue());
	    if (isSetToRouterMode) {
		startTime = System.currentTimeMillis();
		do {
		    try {
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS,
				BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER);
		    } catch (TestException exception) {
			errorMessage = errorMessage
				+ "Exception occurred while setting device Lan Mode to Router Mode => "
				+ exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Device Lan Mode is changed to Router Mode successfully.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s8";
	    status = false;
	    errorMessage = "Bridge mode is not disabled even after 3 minutes of changing the device to bridge mode.";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify whether Bridge mode is disabled via SNMP.");
	    LOGGER.info("STEP 8: ACTION : Execute SNMP Get command for OID: 1.3.6.1.4.1.17270.50.2.3.2.1.1.32");
	    LOGGER.info("STEP 8: EXPECTED : SNMP output should be '2'.");
	    LOGGER.info("********************************************************************************************");
	    startTime = System.currentTimeMillis();
	    do {
		try {
		    status = BroadBandSnmpUtils.performSnmpWalkAndVerify(tapEnv, device,
			    BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(),
			    BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex(),
			    BroadBandTestConstants.STRING_VALUE_TWO);
		} catch (TestException exception) {
		    errorMessage = errorMessage + exception.getMessage();
		    LOGGER.error(errorMessage);
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Device Lan Mode as Router Mode verified successfully via SNMP.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s9";
	    status = false;
	    errorMessage = "Initial & Final Ip tables are not same after Bridge Mode to router transition.";
	    LOGGER.info("*******************************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION: Verify Ip tables after Bridge Mode to Router transition.");
	    LOGGER.info("STEP 9: ACTION: Get the current Ip table rules and compare them with the previous tables.");
	    LOGGER.info("STEP 9: EXPECTED: Ip tables must be same as they are obtained in Step1.");
	    LOGGER.info("*******************************************************************************************");
	    startTime = System.currentTimeMillis();
	    if (!DeviceModeHandler.isFibreDevice(device)) {
		do {
		    try {
			status = BroadBandCommonUtils.getIptableRulesAndVerify(device, tapEnv,
				BroadBandTestConstants.FILE_NAME_TO_STORE_INITIAL_IPV4_TABLE_RULES,
				BroadBandTestConstants.FILE_NAME_TO_STORE_INITIAL_IPV6_TABLE_RULES);
		    } catch (Exception exception) {
			errorMessage = errorMessage
				+ "Exception occurred while verifying Ip tables after Bridge Mode to router transition => "
				+ exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		if (status) {
		    LOGGER.info(
			    "STEP 9: ACTUAL : Initial & Final Ip tables are same after Bridge Mode to router transition.");
		} else {
		    LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    } else {
		errorMessage = "skippig brlan0 bridge mode validations as it is a Fibre device";
		LOGGER.info("STEP 9: ACTUAL : " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    testStepNumber = "s10";
	    status = false;
	    errorMessage = "brlan0 configuration has not IPv4 and IPv6 address fields in Router Mode.";
	    LOGGER.info("*******************************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION: Verify brlan0 configuration in Router mode.");
	    LOGGER.info("STEP 10: ACTION: SSH the device and Execute the following command: 'ifconfig brlan0'.");
	    LOGGER.info("STEP 10: EXPECTED: brlan0 interface must have valid Ip addresses.");
	    LOGGER.info("*******************************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.IFCONFIG_BRLAN);
	    LOGGER.info("Obtained brlan0 configuration from ifconfig command : " + response);
	    try {
		status = BroadBandCommonUtils.validateBrlan0Configuration(device, tapEnv);
	    } catch (TestException exception) {
		errorMessage = errorMessage
			+ "Exception occured while verifying IPv4 and IPv6 addresses in brlan0 configuration in Router Mode. => "
			+ exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : brlan0 configuration has IPv4 and IPv6 address fields in Router Mode.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = "Exception occurred during execution : " + exception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    if (BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device)) {
		status = false;
		errorMessage = "Unable to set the device back in Router Mode.";
		LOGGER.info("#######################################################################################");
		LOGGER.info(
			"POST-CONDITION 1: DESCRIPTION : Verify setting device in LAN Mode as currently in Bridge Mode.");
		LOGGER.info(
			"POST-CONDITION 1: ACTION : Execute the Webpa Set command for following param: Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode and set value as 'router'.");
		LOGGER.info("POST-CONDITION 1: EXPECTED : Device must be set in Router Mode.");
		LOGGER.info("#######################################################################################");
		if (BroadBandCommonUtils.verifyDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device)) {
		    status = false;
		    errorMessage = "Unable to set the device back in Router Mode.";
		    try {
			status = BroadBandCommonUtils.setDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
		    } catch (Exception exception) {
			errorMessage = errorMessage + "Exception occured while setting device in LAN Mode => "
				+ exception.getMessage();
		    }
		    if (status) {
			LOGGER.info("POST-CONDITION 1: ACTUAL : Device is set in Router Mode successfully.");
			LOGGER.info("Waiting For 3 minutes after Changing Device to Router Mode.");
			tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTES);
		    } else {
			LOGGER.error("POST-CONDITION 1: ACTUAL : " + errorMessage);
		    }
		}
		status = false;
		errorMessage = "Unable to delete files created in /tmp folder.";
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION 2: DESCRIPTION : Verify deleting all the created files in /tmp folder.");
		LOGGER.info(
			"POST-CONDITION 2: ACTION : SSH the device and Execute 'rm' command for all the created files in /tmp folder.");
		LOGGER.info("POST-CONDITION 2: EXPECTED : All files created in /tmp folder must be deleted.");
		LOGGER.info("#######################################################################################");
		try {
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			    RDKBTestConstants.CMD_REMOVE_DIR_FORCEFULLY, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_NAME_TO_STORE_FINAL_IPV6_TABLE_RULES));
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			    RDKBTestConstants.CMD_REMOVE_DIR_FORCEFULLY, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_NAME_TO_STORE_INITIAL_IPV6_TABLE_RULES));
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			    RDKBTestConstants.CMD_REMOVE_DIR_FORCEFULLY, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_NAME_TO_STORE_INITIAL_IPV4_TABLE_RULES));
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			    RDKBTestConstants.CMD_REMOVE_DIR_FORCEFULLY, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_NAME_TO_STORE_FINAL_IPV4_TABLE_RULES));
		    status = true;
		    if (status) {
			LOGGER.info(
				"POST-CONDITION 2: ACTUAL : All files created in /tmp folder deleted successfully.");
		    } else {
			LOGGER.error("POST-CONDITION 2: ACTUAL : " + errorMessage);
		    }
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred while deleting files in /tmp folder.");
		    errorMessage = exception.getMessage();
		    LOGGER.error(errorMessage);
		}
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	    LOGGER.info("ENDING TEST CASE: TC-RDKB-BRIDGE-MODE-1003");
	}
    }

}