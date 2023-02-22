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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestCategory;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandSnmpConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.RdkBWifiParameters;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.interfaces.FactoryResetSettings;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRestoreWifiUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.LoggerUtils;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.AdvancedFeatureServices;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.CodeBig;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.ForceWiFiDisable;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.LimitBeaconDetectionEnum;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.LocalGatewayIPv4;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.ParentalControlService;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.RabidMemoryLimit;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.TelemetryEndpoint;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.WifiSettings2Ghz;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.WifiSsid;
import com.automatics.rdkb.utils.factoryreset.BroadBandFactoryResetUtils.WifiSsidSnmp;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.tr69.BroadBandTr69Utils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandFactoryResetTests extends AutomaticsTestBase {

	private Map<String, String> defaultValuesForSSID = null;

	/**
	 *
	 * Test Case # 1: Perform Factory Reset Using WebPA.
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRECONDITION 1 - Get Default SSID value for 2.4Ghz and 5 Ghz using WebPA
	 * request.</li>
	 * <li>PRECONDITION 2 - Get Default value for Parental Control Services.</li>
	 * <li>PRECONDITION 3 - Get Default value for Advanced feature Services.</li>
	 * <li>PRECONDITION 4 -Get Default value for LOCAL Gateway IPv4, DHCP LOCAL IPv4
	 * Start, DHCP LOCAL IPv4 End</li>
	 * <li>PRECONDITION 5 -Set Default value for 2.4GHz Channel bandwidth as 40
	 * MHz</li>
	 * <li>Step 1 - Verify setting SSID Name value other than default SSID Name of
	 * 2.4Ghz and 5 Ghz using WebPA request.</li>
	 * <li>Step 2 -Modify Parental Control service value other than default
	 * value.</li>
	 * <li>Step 3 -Modify Advanced feature service value other than default
	 * value.</li>
	 * <li>Step 4 -Modify Local Gateway IPV4, DHCP start and end value.</li>
	 * <li>Step 5 -Verify Changing 2.4 GHz channel bandwidth to value other than
	 * 20/40 MHz.</li>
	 * <li>Step 6 -Verify SSID value of 2.4Ghz and 5 Ghz with default SSID value
	 * after Factory Reset.</li>
	 * <li>Step 7 -Verify Parental Control service value with the default value
	 * <li>Step 8 -Verify Advanced feature service value with default value. .</li>
	 * <li>Step 9 - Verify Local Gateway IPV4, DHCP start and end value with default
	 * value.</li>
	 * <li>Step 10 : Verify the Current SSID value for 2.4 Ghz using SNMP
	 * request.</li>
	 * <li>Step 11 : Verify 2.4 GHz channel bandwidth value is 20/40 MHz.
	 * <li>
	 * <li>Step 12 : Verify 5GHZ public Home SSID is disabled through WebPA request.
	 * <li>
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-FACTORYRBT-1002")
	public void testToVerifyFactoryResetUsingWebpa(Dut device) {
		String testCaseId = "TC-RDKB-FACTORYRBT-002";
		testToVerifyFactoryReset(device, testCaseId, TestCategory.WEBPA);
	}

	/**
	 *
	 * Test Case # 2: Perform Factory Reset Using SNMP.
	 * 
	 * <ol>
	 * <li>Pre-Condition 1: Verify WAN status through sysevent.</li>
	 * <li>Pre-Condition 2: Verify getting default SSID values for 2.4GHz and 5GHz
	 * using WebPA request.</li>
	 * <li>Pre-Condition 3: Verify getting default SSID values for both 2.4GHz &
	 * 5GHz via SNMP.</li>
	 * <li>Pre-Condition 4: Verify getting default values for Parental Control
	 * Services.</li>
	 * <li>Pre-Condition 5: Verify getting default values for Advanced feature
	 * Services.</li>
	 * <li>Pre-Condition 6: Verify getting default values for LOCAL Gateway IPv4,
	 * DHCP LOCAL IPv4 Start, DHCP LOCAL IPv4 End.</li>
	 * <li>Pre-Condition 7: Verify getting default value for 2.4GHz Channel
	 * bandwidth.</li>
	 * <li>Pre-Condition 8: Verify CodeBig Enable is disabled by default.</li>
	 * <li>Pre-Condition 9: Verify Telemetry Endpoint is disabled by default.</li>
	 * <li>Pre-Condition 10: Verify getting default value for
	 * LimitBeaconDetection.</li>
	 * <li>Pre-Condition 11: Verify getting default value for rabid framework memory
	 * limit.</li>
	 * <li>Pre-Condition 12: Verify force wifi disable is disabled by default.</li>
	 * <li>Verify device uptime is more than 20 minutes.</li>
	 * <li>Verify setting SSID Name value other than default SSID Name of 2.4Ghz and
	 * 5 Ghz using WebPA request.</li>
	 * <li>Verify modifying Parental Control service value other than default
	 * value.</li>
	 * <li>Verify Modifying Advanced feature service value other than default
	 * value.</li>
	 * <li>Verfiy Modifying Local Gateway IPV4, DHCP start and end value other than
	 * default value.</li>
	 * <li>Verify Changing 2.4 GHz channel bandwidth to value other than 20/40
	 * MHz.</li>
	 * <li>Verify Enabling Telemetry Endpoint through WebPA request.</li>
	 * <li>Verify Setting LimitBeaconDetection to true.</li>
	 * <li>Verify Changing rabid framework memory limit to 10MB.</li>
	 * <li>Verify setting Force WiFi Disable to true.</li>
	 * <li>Verify Performing Factory reset on the device using SNMP request.</li>
	 * <li>Verify the Current SSID value for 2.4Ghz and 5 Ghz using WebPA
	 * request.</li>
	 * <li>Verify Parental Control service value with the default value.</li>
	 * <li>Verify Advanced feature service value with default value.</li>
	 * <li>Verify Local Gateway IPV4, DHCP start and end value with default
	 * value.</li>
	 * <li>Verify the Current SSID value for 2.4Ghz and 5 Ghz using SNMP
	 * request.</li>
	 * <li>Verify 2.4 GHz channel bandwidth value is 20/40 MHz.</li>
	 * <li>Verify that the CodeBigFirst Enable has default value.</li>
	 * <li>Verify that the TelemetryEndpoint Enable has default value.</li>
	 * <li>Verify the default value of LimitBeaconDetection is false after Factory
	 * Reset.</li>
	 * <li>Verify that the Rabid framework memory limit has default value.</li>
	 * <li>Verify that Force WiFi Disable has default value.</li>
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-FACTORYRBT-1003")
	public void testToVerifyFactoryResetUsingSnmp(Dut device) {
		// Test case id declaration
		String testCaseId = "TC-RDKB-FACTORYRBT-003";
		testToVerifyFactoryReset(device, testCaseId, TestCategory.SNMP);
	}

	/**
	 * Test to verify Default SSID and Password after Factory Resetting Router
	 * through WebPA
	 * 
	 * <ol>
	 * <li>Pre-Condition 1: Verify whether WebPA is up and running in the
	 * Device.</li>
	 * <li>Pre-Condition 2: Verify getting default SSID values for 2.4GHz & 5GHz via
	 * WebPa.</li>
	 * <li>Verify performing Factory Reset via WebPa and wait for device to come
	 * up.</li>
	 * <li>Verify whether WebPA is up and running in the device after successful
	 * Factory Reset.</li>
	 * <li>Verify last reboot reason.</li>
	 * <li>Verify the default value of 2.4GHz SSID.</li>
	 * <li>Verify default value of 2.4GHz SSID password.</li>
	 * <li>Verify the default value of 5GHz SSID.</li>
	 * <li>Verify default value of 5GHz SSID password.</li>
	 * <li>Post-Condition 1: Perform device recativation.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Revanth Kumar Vella
	 * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-FACTORYRBT-1006")
	public void testToVerifyDefaultSSIDAfterFactoryResettingRouter(Dut device) {
		// Variable declaration starts
		String testCaseId = "TC-RDKB-FACTORYRBT-106";
		String stepNum = null;
		String errorMessage = null;
		String response = null;
		boolean status = false;
		boolean isFactoryResetDone = false;
		boolean isMeshEnabled = false;
		boolean isMeshEnableInDb = false;
		boolean isMeshEnableInLog = false;
		boolean isBandSteeringEnabled = false;
		BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
		// Variable declaration ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FACTORYRBT-1006");
		LOGGER.info("TEST DESCRIPTION: Verify Default SSID and Password after Factory Resetting Router through WebPA");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("Pre-Conditon 1: Verify whether WebPA is up and running in the Device.");
		LOGGER.info("Pre-Conditon 2: Verify getting default SSID values for 2.4GHz & 5GHz via WebPa.");
		LOGGER.info("1. Verify performing Factory Reset via WebPa and wait for device to come up.");
		LOGGER.info("2. Verify whether WebPA is up and running in the device after successful Factory Reset.");
		LOGGER.info("3.1. Verify Mesh status is enabled in db using syscfg");
		LOGGER.info("3.2. Verify Default value of Mesh after Factory Reset via webpa/dmcli");
		LOGGER.info("4. Verify Default value of BS after Factory Reset via webpa/dmcli");
		LOGGER.info("5. Verify last reboot reason.");
		LOGGER.info("6. Verify the default value of 2.4GHz SSID.");
		LOGGER.info("7. Verify default value of 2.4GHz SSID password.");
		LOGGER.info("8. Verify the default value of 5GHz SSID.");
		LOGGER.info("9. Verify default value of 5GHz SSID password.");
		LOGGER.info("Post-Conditon 1:  Perform device recativation.");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			errorMessage = "Webpa is not up and Running.";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Verify whether WebPA is up and running in the Device.");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : Verify Successful webpa Get response ,in case of failure rechecking for 8 minutes.");
			LOGGER.info("PRE-CONDITION 1 : EXPECTED : WebPA should be up and running in the Device.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : Webpa process is up and running successfully.");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : Webpa process is not  up and running so precondition failed");
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			// DSL device has no specific pattern for default SSID unlike other broadband
			// devices.
			// So Getting the default SSID before factory reset and verify its the same
			// after factory reset
			if (DeviceModeHandler.isDSLDevice(device)) {
				errorMessage = "Unable to get default values for SSID(2.4GHz & 5GHz).";
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"PRE-CONDITION 2 : DESCRIPTION : Verify getting default SSID values for 2.4GHz & 5GHz via WebPa.");
				LOGGER.info(
						"PRE-CONDITION 2 : ACTION : Execute WebPa Get command for following params: Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID & Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID");
				LOGGER.info(
						"PRE-CONDITION 2 : EXPECTED : WebPa Get command should execute successfully and return default values.");
				LOGGER.info("**********************************************************************************");
				populateDefaultValuesForSSID(device, tapEnv);
				if (status) {
					LOGGER.info("PRE-CONDITION 2 : ACTUAL : Successfully got default values for both the SSIDs.");
				} else {
					LOGGER.error("PRE-CONDITION 2 : ACTUAL : Pre condition failed");
					throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
			} else {
				LOGGER.info("Pre-Condition 2 is only applicable for DSL devices.");
			}

			stepNum = "S1";
			errorMessage = "Unable to perform Factory Reset on the device.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify performing Factory Reset via WebPa and wait for device to come up.");
			LOGGER.info(
					"STEP 1: ACTION : Execute WebPa Set command for following param: Device.X_CISCO_COM_DeviceControl.FactoryReset and set value as 'Router' and wait for device to come up.");
			LOGGER.info(
					"STEP 1: EXPECTED : Factory reset should be successful and device should go for reboot and also should come up later.");
			LOGGER.info("**********************************************************************************");
			isFactoryResetDone = status = BroadBandFactoryResetUtils.methodToPerformFactoryResetObjectAndDeviceToComeUp(
					tapEnv, device, BroadBandTestConstants.STRING_ROUTER);
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL: Factory Reset performed successfully, Device went for reboot and comes up after that.");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S2";
			errorMessage = "WebPa is not coming up after successful Factory Reset even after waiting for 8 minutes.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify whether WebPA is up and running in the device after successful Factory Reset.");
			LOGGER.info(
					"STEP 2: ACTION : Verify Successful webpa Get response ,in case of failure rechecking for 8 minutes.");
			LOGGER.info(
					"STEP 2: EXPECTED : WebPA should be up and running in the Device after successful Factory Reset.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully verified webpa is up and running");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s3.1";
			errorMessage = "MESH enabled value obtained in log file is not appearing similar to obtained in syscfg db";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3.1: DESCRIPTION : Verify Mesh status in bootup WEBPAlog file & syscfg db");
			LOGGER.info("STEP 3.1: ACTION : Execute in log file MeshAgentLog.txt.0 and syscfg show| grep -i mesh");
			LOGGER.info("STEP 3.1: EXPECTED : Value of Mesh should be same across ");
			LOGGER.info("**********************************************************************************");
			// mesh value in boot up webpa logs
			String isMeshEnableInLogFile = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
					tapEnv, BroadBandTestConstants.MESHWIFI_ENABLED_LOG, BroadBandTestConstants.LOG_FILE_MESHAGENT,
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS);
			if (CommonMethods.isNotNull(isMeshEnableInLogFile)) {
				isMeshEnableInLog = true;
			} else {
				isMeshEnableInLog = false;
			}

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_MESH_VALUE_SYSCFG_DB);
			isMeshEnableInDb = CommonMethods.isNotNull(response) && CommonMethods.isGivenStringAvailableInCommandOutput(
					response, BroadBandTestConstants.MESH_SYSCFG_DB_OP + BroadBandTestConstants.TRUE);

			status = isMeshEnableInLog == isMeshEnableInDb;

			if (status) {
				LOGGER.info("STEP 3.1: ACTUAL : MESH is " + isMeshEnableInLog
						+ " in bootup webpa logs (via xpc) & reflecting same value in syscfg db");
			} else {
				LOGGER.error("STEP 3.1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			LOGGER.info("****************************************************************");

			// 3.2
			/**
			 * Step 3.2 : Verify Default value of Mesh after Factory Reset via webpa/dmcli
			 */
			stepNum = "s3.2";
			status = false;
			errorMessage = "Failed to validate Mesh default value after Factory Reset";
			LOGGER.info("**************************************************************************");
			LOGGER.info("STEP 3.2 : DESCRIPTION  : Verify Default value of Mesh after Factory Reset via webpa/dmcli");
			LOGGER.info(
					"STEP 3.2 : ACTION : Execute dmcli cmd Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable");
			LOGGER.info(
					"STEP 3.2 : EXPECTED: Default value of Mesh after FR should be True for Atom based devices with wifi extender, fibre devices and a specific arm based device");
			LOGGER.info("**************************************************************************");
			String meshWebpaValue = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
			if (CommonMethods.isNotNull(meshWebpaValue)
					&& meshWebpaValue.equalsIgnoreCase(BroadBandTestConstants.TRUE)) {
				isMeshEnabled = true;
			}

			if ((isMeshEnabled == isMeshEnableInLog) || (isMeshEnabled == isMeshEnableInDb)) {
				status = true;
			}

			if (status) {
				LOGGER.info(
						"STEP 3.2: ACTUAL: Successfully verified default value of MESH is same as in Logs & syscfg db after Factory Reset");
			} else {
				LOGGER.info("STEP 3.2: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			LOGGER.info("****************************************************************");

			/** Step 4 : Verify Default value of BS after Factory Reset via webpa/dmcli */
			stepNum = "s4";
			status = false;
			errorMessage = "Failed to validate BS default value after Factory Reset";
			LOGGER.info("**************************************************************************");
			LOGGER.info("STEP 4 : DESCRIPTION  : Verify Default value of BS after Factory Reset via webpa/dmcli");
			LOGGER.info("STEP 4 : ACTION : Execute dmcli cmd Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable");
			LOGGER.info("STEP 4 : EXPECTED: Default value of BS after FR should be False");
			LOGGER.info("**************************************************************************");
			if (isMeshEnabled) {
				isBandSteeringEnabled = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE, BroadBandTestConstants.FALSE);
			} else {
				response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE);
				LOGGER.info("Since Mesh is not enabled, default value of BS can be true/false");
				isBandSteeringEnabled = true;
			}
			if (isBandSteeringEnabled) {
				LOGGER.info(
						"STEP 4: ACTUAL: Successfully verified default value of BS if MESH is Enabled (Band Steering should be disabled)");
			} else {
				LOGGER.info("STEP 4: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, isBandSteeringEnabled, errorMessage, false);
			LOGGER.info("****************************************************************");

			stepNum = "S5";
			errorMessage = "Unable to verify last reboot reason.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify last reboot reason.");
			LOGGER.info(
					"STEP 5: ACTION : Execute the WebPa Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason.");
			LOGGER.info("STEP 5: EXPECTED : Last reboot reason should be 'factory-reset'.");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON);
			status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
					BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET, response);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL: Last reboot reason is verified successfully as 'factory-reset'.");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S6";
			errorMessage = "Unable to verify default 2.4GHz SSID value.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify the default value of 2.4GHz SSID.");
			LOGGER.info(
					"STEP 6: ACTION : Execute the WebPa Get command for following param: Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID");
			LOGGER.info(
					"STEP 6: EXPECTED : WebPa Get comamnd should execute successfully and should return default 2.4GHz SSID value.");
			LOGGER.info("**********************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ);
				status = verifySSIDStatus(BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ, response);
			} else {
				broadBandResultObject = BroadBandRestoreWifiUtils.verifyDefaultSsidForAllPartners(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Default 2.4GHz SSID value verified successfully.");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S7";
			errorMessage = "Unable to verify default 2.4GHz SSID password value.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify default value of 2.4GHz SSID password.");
			LOGGER.info(
					"STEP 7: ACTION : Execute the WebPa Get command for following param: Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_DefaultKeyPassphrase");
			LOGGER.info(
					"STEP 7: EXPECTED : WebPa Get comamnd should execute successfully and should return default 2.4GHz password value.");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_2_4);
			status = CommonMethods.isNotNull(response);
			if (status) {
				LOGGER.info("STEP 7: ACTUAL: Default 2.4GHz SSID password value verified successfully.");
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S8";
			errorMessage = "Unable to verify default 5GHz SSID value.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify the default value of 5GHz SSID.");
			LOGGER.info(
					"STEP 8: ACTION : Execute the WebPa Get command for following param: Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID");
			LOGGER.info(
					"STEP 8: EXPECTED : WebPa Get comamnd should execute successfully and should return default 5GHz SSID value.");
			LOGGER.info("**********************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ);
				status = verifySSIDStatus(BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ, response);
			} else {
				broadBandResultObject = BroadBandRestoreWifiUtils.verifyDefaultSsidForAllPartners(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info("STEP 8: ACTUAL: Default 5GHz SSID value verified successfully.");
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S9";
			errorMessage = "Unable to verify default 5GHz SSID password value.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify default value of 5GHz SSID password.");
			LOGGER.info(
					"STEP 9: ACTION : Execute the WebPa Get command for following param: Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_DefaultKeyPassphrase");
			LOGGER.info(
					"STEP 9: EXPECTED : WebPa Get comamnd should execute successfully and should return default 5GHz password value.");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_5);
			status = CommonMethods.isNotNull(response);
			if (status) {
				LOGGER.info("STEP 9: ACTUAL: Default 5GHz SSID password value verified successfully.");
			} else {
				LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			if (isFactoryResetDone) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				LOGGER.info("POST-CONDITION STEPS");

				LOGGER.info("############################################################################");
				LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Perform device recativation.");
				LOGGER.info(
						"POST-CONDITION 1 : ACTION : Execute the device recativation sequence using webpa commands.");
				LOGGER.info("POST-CONDITION 1 : EXPECTED : Reactivation should be successfully performed.");
				LOGGER.info("############################################################################");
				try {
					BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
					status = true;
				} catch (Exception exception) {
					errorMessage = "Exception occured while reactivating the device." + exception.getMessage();
				}
				if (status) {
					LOGGER.info("POST-CONDITION 1 : ACTUAL : Device Reactivated successfully.");
				} else {
					LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
				}

				LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FACTORYRBT-1006");
	}

	/**
	 * Verify Default SSID and Password after Factory Resetting Wifi through WebPA
	 * object
	 * <ol>
	 * <li>Factory reset with value Wifi.</li>
	 * <li>Verify whether device becomes factory reset and verify whether device
	 * becomes accessible.</li>
	 * <li>Verify Default SSID after Factory Resetting Wifi through WebPA object for
	 * 2.4Ghz</li>
	 * <li>Verify Default Password after Factory Resetting Wifi through WebPA object
	 * for 2.4Ghz</li>
	 * <li>Verify Default SSID after Factory Resetting Wifi through WebPA object for
	 * 5Ghz</li>
	 * <li>Verify Default Password after Factory Resetting Wifi through WebPA object
	 * for 5Ghz</li>
	 * 
	 * @param device {@link Dut}
	 * @author Revanth Kumar Vella
	 * @refactor Athira
	 *           </ol>
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-FACTORYRBT-1007")
	public void FactoryResettingWiFi(Dut device) {

		// Variable Declaration begins
		String testCaseId = "";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		// long value to store start time of polling period
		long startTime = BroadBandTestConstants.CONSTANT_0;
		// Variable Declation Ends

		testCaseId = "TC-RDKB-FACTORYRBT-107";

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FACTORYRBT-1007");
		LOGGER.info(
				"TEST DESCRIPTION: Verify Default SSID and Password after Factory Resetting Wifi through WebPA object ");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Factory reset  with value Wifi. ");
		LOGGER.info("2. Verify whether device becomes factory reset and verify whether device becomes accessible.");
		LOGGER.info("3. Verify Default SSID after Factory Resetting Wifi through WebPA object for 2.4Ghz");
		LOGGER.info("4. Verify Default Password after Factory Resetting Wifi through WebPA object for 2.4Ghz");
		LOGGER.info("5. Verify Default SSID after Factory Resetting Wifi through WebPA object for 5Ghz");
		LOGGER.info("6. Verify Default Password after Factory Resetting Wifi through WebPA object for 5Ghz");

		LOGGER.info("#######################################################################################");
		try {

			LOGGER.info("**********************************************************************************");

			// DSL device has no specific pattern for default SSID unlike other broadband
			// devices.
			// So Get the default SSID before factory reset and verify its the same after
			// factory reset
			/**
			 * PRECONDITION 1 :Get Default SSID value for 2.4Ghz and 5 Ghz using WebPA
			 * parameter
			 */
			LOGGER.info("*****************************************************************************************");
			LOGGER.info(
					"PRE-CONDITION 1 DESCRIPTION:  GET DEFAULT SSID VALUE FOR 2.4GHZ AND 5 GHZ USING WEBPA PARAMETER ");
			LOGGER.info("PRE-CONDITION 2 ACTION : EXECUTE Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID and "
					+ "Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID TO GET DEFAULT SSID VALUE ");
			LOGGER.info(
					"PRE-CONDITION 1 EXPTECTED: WEBPA REQUEST SHOULD RETURN DEFAULT SSID VALUE WITH SUCCESS MESSAGE ");
			LOGGER.info("***********************************************************************************");
			if (DeviceModeHandler.isDSLDevice(device)) {
				populateDefaultValuesForSSID(device, tapEnv);
			}

			stepNum = "s1";
			errorMessage = "Failed to factory reset the device with value WiFi.";
			status = false;
			String timeStamp = null;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Factory reset  with value Wifi. ");
			LOGGER.info("STEP 1: ACTION : Set webpa Device.X_CISCO_COM_DeviceControl.FactoryReset with value Wifi.");
			LOGGER.info("STEP 1: EXPECTED : Factory reset should be successful.");
			LOGGER.info("**********************************************************************************");
			try {
				timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
				status = BroadBandFactoryResetUtils.performFactoryResetWebPaForWifi(tapEnv, device, "Wifi");
				if (status) {
					LOGGER.info("STEP 1: ACTUAL : FACTORY RESET IS SUCCESS WITH VALUE WIFI.");
				} else {
					LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
				}
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			} catch (TestException exception) {
				errorMessage = "Exception occured while Factory Reset with value Wifi : " + exception.getMessage();
				LOGGER.error(errorMessage);
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			}

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "The string \"FactoryReset:CosaDmlDcSetFactoryReset Restoring WiFi to factory defaults\" is not  present in PAMlog.txt.0. ";
			status = false;
			String response = null;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify whether device becomes factory reset and verify whether device becomes accessible.");
			LOGGER.info(
					"STEP 2: ACTION : Check Wifilog file and wait for a maximum of five minutes for the device to come up after factory reset.");
			LOGGER.info(
					"STEP 2: EXPECTED : The string \"FactoryReset:CosaDmlDcSetFactoryReset Restoring WiFi to factory defaults\" should present in PAMlog.txt.0 file. And device should accessible");
			LOGGER.info("**********************************************************************************");
			try {
				startTime = System.currentTimeMillis();
				do {
					response = LoggerUtils.getLatestLogMessageBasedOnTimeStamp(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_FACTORY_RESET_WIFI,
							BroadBandTestConstants.COMMAND_NTP_LOG_FILE, timeStamp, false);
					status = CommonMethods.isNotNull(response);
				} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS && !status
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				if (status) {
					LOGGER.info("STEP 2: ACTUAL : DEVICE IS UP AFTER FACTORY RESET.");
				} else {
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
			} catch (TestException exception) {
				errorMessage = "Exception occured while wait till the device becomes accessible : "
						+ exception.getMessage();
				LOGGER.error(errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Default SSID is not obtained as expected from Webpa "
					+ BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ;
			status = false;
			String defaultSSID = null;
			BroadBandResultObject broadBandResultObject = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Verify Default SSID after Factory Resetting Wifi through WebPA object for 2.4Ghz");
			LOGGER.info("STEP 3: ACTION : Execute webpa Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID ");
			LOGGER.info("STEP 3: EXPECTED : Expected device's default SSID for 2.4Ghz should be displayed");
			LOGGER.info("**********************************************************************************");

			try {
				response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ);
				if (DeviceModeHandler.isDSLDevice(device)) {
					status = verifySSIDStatus(BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ, response);
				} else {
					broadBandResultObject = BroadBandRestoreWifiUtils.verifyDefaultSsidForAllPartners(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ);
					status = broadBandResultObject.isStatus();
					errorMessage = broadBandResultObject.getErrorMessage();
				}
			} catch (TestException e) {
				status = false;
				errorMessage += e.getMessage();
				LOGGER.error("Error Occured While Validating Default SSID :" + errorMessage);
			}

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : VERIFIED THE DEFAULT SSID FOR 2.4 GHZ AS " + response);
			} else {
				errorMessage = errorMessage + " Received Webpa  response is : " + response;
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Failed to get default password using "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_2_4;
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify Default Password after Factory Resetting Wifi through WebPA object for 2.4Ghz.");
			LOGGER.info(
					"STEP 4: ACTION : Execute webpa Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_DefaultKeyPassphrase. ");
			LOGGER.info("STEP 4: EXPECTED : Expected device's default Password for 2.4Ghz should be displayed.");
			LOGGER.info("**********************************************************************************");

			try {
				response = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_2_4);
				status = CommonMethods.isNotNull(response);
			} catch (TestException e) {
				status = false;
				errorMessage += e.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : VERIFIED THE DEFAULT PASSWORD FOR 2.4 GHZ AS " + response);
			} else {
				errorMessage = errorMessage + " Received Webpa  response is : " + response;
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "Default SSID is not obtained as expected from Webpa "
					+ BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ;
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Verify default SSID after Factory Resetting Wifi through WebPA object for 5Ghz.");
			LOGGER.info("STEP 5: ACTION : Execute webpa Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID. ");
			LOGGER.info("STEP 5: EXPECTED : Expected device's default SSID for 5Ghz should be displayed.");
			LOGGER.info("**********************************************************************************");

			try {
				response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ);
				if (DeviceModeHandler.isDSLDevice(device)) {
					status = verifySSIDStatus(BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ, response);
				} else {
					broadBandResultObject = BroadBandRestoreWifiUtils.verifyDefaultSsidForAllPartners(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ);
					status = broadBandResultObject.isStatus();
					errorMessage = broadBandResultObject.getErrorMessage();
				}

			} catch (TestException e) {
				status = false;
				errorMessage += e.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : VERIFIED THE DEFAULT SSID FOR 5 GHZ AS " + response);
			} else {
				errorMessage = errorMessage + " Received Webpa  response is : " + response;
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "Failed to get default password using "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_5;
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify Default Password after Factory Resetting Wifi through WebPA object for 5Ghz.");
			LOGGER.info(
					"STEP 6: ACTION : Execute webpa Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_DefaultKeyPassphrase. ");
			LOGGER.info("STEP 6: EXPECTED : Expected Device's default Password for 5Ghz should be displayed.");
			LOGGER.info("**********************************************************************************");

			try {
				response = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_5);
				status = CommonMethods.isNotNull(response);
			} catch (TestException e) {
				status = false;
				errorMessage += e.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : VERIFIED THE DEFAULT PASSWORD FOR 5 GHZ AS " + response);
			} else {
				errorMessage = errorMessage + " Received Webpa  response is : " + response;
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
			;
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Perform device recativation");
			LOGGER.info("POST-CONDITION : ACTION : Execute the device recativation sequence using webpa commands");
			LOGGER.info("POST-CONDITION : EXPECTED : reactivation  should be successfully performed ");
			try {
				BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
				status = true;
			} catch (Exception exception) {
				errorMessage = "EXCEPTION OCCURED WHILE REACTIVATE THE DEVICE " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"POST-CONDITION : ACTUAL : POST CONDITION EXECUTED SUCCESSFULLY. REACTIVATED THE DEVICEAFTER FACTORY RESET.");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FACTORYRBT-1007");
	}

	/**
	 * Test method used to get and verify the Wan IP and Client IP
	 * 
	 * @param device                   Instance of {@link Dut}
	 * @param testId                   Test case ID
	 * @param stepNumber               Step Number
	 * @param initialCommonParamValues Key and pair value mapping
	 * @param isCompare                True- Compare with initial values
	 * @throws TestException
	 * @refactor Govardhan
	 */
	public static Map<String, String> executeTestSeptsToGetAndVerifyWanIPAndClientIP(Dut device, String testId,
			int stepNumber, Map<String, String> initialCommonParamValues, boolean isCompare) throws TestException {
		String step = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String successMessage = null;
		Map<String, String> commonParamValues = null;
		/**
		 * Step : RETRIEVE THE WAN IP AND CLIENT IP ROUTER VALUES FROM DEVICE
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Retrieve WAN IP and Client IP Router values from device");
		LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
				+ "	1. Get the value for WAN IPv4 Address \n" + "	2. Get the value for DHCP v4 client IP routers");
		LOGGER.info(
				"STEP " + stepNumber + " : EXPECTED : Must return the WAN IP and Client IP Router values from device.");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to retrieve WAN IP and Client IP Router values from device.";
		try {
			commonParamValues = BroadBandWebPaUtils.executeAndGetListOfWebParameters(device, tapEnv,
					BroadBandWebPaConstants.WEB_PARAM_FOR_WAN_CLIENT_IP);
			status = !commonParamValues.isEmpty();
			successMessage = "Sucessfully retrieved the WAN IP and Client IP Router values from device";

			if (status) {
				status = false;
				if (isCompare) {
					LOGGER.info("Comparing the WAN IP and Client IP Router with initial values");
					errorMessage = "Unable to cross verify the WAN IP and Client IP Router values with initial WAN IP and Client IP Router values.";
					status = BroadBandWebPaUtils.verifyWebpaGetResponseValues(device, tapEnv, initialCommonParamValues,
							commonParamValues);
					successMessage = "Sucessfully cross verified the WAN IP and Client IP Router values with initial WAN IP and Client IP Router values.";
				} else {
					LOGGER.info("Verifying the WAN IP and Client IP Router are empty or null");
					status = BroadBandWebPaUtils.getAndVerifyMapValueIsNotNullOrEmpty(commonParamValues);
				}
			}
		} catch (Exception e) {
			errorMessage += e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : " + successMessage);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
		return commonParamValues;
	}

	/**
	 * Test method used to set and verify the wifi radio values
	 * 
	 * @param device     Instance of {@link Dut}
	 * @param testId     Test case ID
	 * @param stepNumber Step Number
	 * @throws TestException
	 * @refactor Govardhan
	 */
	public static Map<String, String> executeTestSeptsToSetTheRadioValues(Dut device, String testId, int stepNumber)
			throws TestException {
		String step = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		Map<String, String> setValuesMap = null;
		/**
		 * Step : SET AND VERIFY THE 2.4 & 5 GHZ RADIO VALUES FROM DEVICE
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP " + stepNumber + " : DESCRIPTION : Set and verify the value for 2.4 & 5 GHz Radios in device");
		LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
				+ "1. Get the value for 2.4 GHz radio extension channel \n"
				+ "2. Get the value for 2.4 GHz radio beacon interval \n"
				+ "3. Get the value for 2.4 GHz radio basic rate \n"
				+ "4. Get the value for 2.4 GHz radio operating standard \n"
				+ "5. Get the value for 2.4 GHz radio transmit power \n"
				+ "6. Get the value for 2.4 GHz radio status \n" + "7. Get the value for 2.4 GHz radio channel \n"
				+ "8. Get the value for 2.4 GHz radio wireless channel \n"
				+ "9. Get the value for 2.4 GHz radio operating channel bandwidth \n"
				+ "10. Get the value for 2.4 GHz radio dfs enable \n"
				+ "11. Get the value for 5 GHz radio extension channel \n"
				+ "12. Get the value for 5 GHz radio beacon interval \n"
				+ "13. Get the value for 5 GHz radio basic rate \n"
				+ "14. Get the value for 5 GHz radio operating standard \n"
				+ "15. Get the value for 5 GHz radio transmit power \n" + "16. Get the value for 5 GHz radio status \n"
				+ "17. Get the value for 5 GHz radio channel \n"
				+ "18. Get the value for 5 GHz radio wireless channel \n"
				+ "19. Get the value for 5 GHz radio operating channel bandwidth \n"
				+ "20. Get the value for 5 GHz radio dfs enable ");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must set the value for 2.4 & 5 GHz Radios in device.");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to set and verify the 2.4 & 5 GHz Radios values in device.";
		setValuesMap = BroadBandWebPaUtils.setTheWebParamForWifiRadios(device, tapEnv, null);
		status = (setValuesMap != null && !setValuesMap.isEmpty());
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Successfully set and verified the value for 2.4 & 5 GHz Radios in device");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
		return setValuesMap;
	}

	/**
	 * Test method used to set and verify the common parameter values
	 * 
	 * @param device     {@link Dut}
	 * @param stepNumber Step Number
	 * @throws TestException
	 * @refactor Govardhan
	 */
	public static Map<String, String> executeTestSeptsToSetTheCommonParameterValues(Dut device, String testId,
			int stepNumber) throws TestException {
		String step = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		Map<String, String> setValuesMap = null;
		/**
		 * Step : SET AND VERIFY THE COMMON PARAMETER VALUES FROM DEVICE
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Set and verify the common parameter values in device");
		LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
				+ "	1. Set the value for DMZ LAN IP Address \n" + "	2. Set the value for Device cloud UI status \n"
				+ "	3. Set the value for device control router enable status \n"
				+ "	4. Set the value for port forwarding status");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must set the common parameter values in device.");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to set and verify the common parameter values in device.";
		setValuesMap = BroadBandWebPaUtils.setTheCommonWebParams(device, tapEnv, null, false);
		status = (setValuesMap != null && !setValuesMap.isEmpty());
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : Successfully set and verified the common parameter values in device");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
		return setValuesMap;
	}

	/**
	 * Test method used to get and verify the wifi radio values
	 * 
	 * @param device                 {@link Dut}
	 * @param testId                 Test case ID
	 * @param stepNumber             Step Number
	 * @param initialWiFiRadioValues Key and pair value mapping
	 * @param isCompare              True- Compare with initial values
	 * @throws TestException
	 * @refactor Govardhan
	 */
	public static void executeTestSeptsToGetAndVerifyRadioValues(Dut device, String testId, int stepNumber,
			Map<String, String> initialWiFiRadioValues, boolean isCompare) throws TestException {
		String step = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String successMessage = null;
		Map<String, String> wifiRadioValues = null;
		/**
		 * Step : RETRIEVE THE 2.4 & 5 GHZ RADIO VALUES FROM DEVICE
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Retrieve value for 2.4 & 5 GHz Radios from device");
		LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
				+ "1. Get the value for 2.4 GHz radio extension channel \n"
				+ "2. Get the value for 2.4 GHz radio beacon interval \n"
				+ "3. Get the value for 2.4 GHz radio basic rate \n"
				+ "4. Get the value for 2.4 GHz radio operating standard \n"
				+ "5. Get the value for 2.4 GHz radio transmit power \n"
				+ "6. Get the value for 2.4 GHz radio status \n" + "7. Get the value for 2.4 GHz radio channel \n"
				+ "8. Get the value for 2.4 GHz radio wireless channel \n"
				+ "9. Get the value for 2.4 GHz radio operating channel bandwidth \n"
				+ "10. Get the value for 2.4 GHz radio dfs enable \n"
				+ "11. Get the value for 5 GHz radio extension channel \n"
				+ "12. Get the value for 5 GHz radio beacon interval \n"
				+ "13. Get the value for 5 GHz radio basic rate \n"
				+ "14. Get the value for 5 GHz radio operating standard \n"
				+ "15. Get the value for 5 GHz radio transmit power \n" + "16. Get the value for 5 GHz radio status \n"
				+ "17. Get the value for 5 GHz radio channel \n"
				+ "18. Get the value for 5 GHz radio wireless channel \n"
				+ "19. Get the value for 5 GHz radio operating channel bandwidth \n"
				+ "20. Get the value for 5 GHz radio dfs enable ");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must return the 2.4 & 5 GHz Radios values from device.");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to retrieve the 2.4 & 5 GHz Radios values from device.";
		try {
			wifiRadioValues = BroadBandWebPaUtils.executeAndGetListOfWebParameters(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMS_FOR_BOTH_RADIOS);
			status = !wifiRadioValues.isEmpty();
			successMessage = "Sucessfully retrieved the 2.4 & 5 GHz Radios values from device";
			if (status) {
				status = false;
				if (isCompare) {
					LOGGER.info("Comparing the wifiRadioValues with initial values");
					errorMessage = "Unable to cross verify the 2.4 & 5 GHz Radios values with 2.4 & 5 GHz Radios values.";
					status = BroadBandWebPaUtils.verifyWebpaGetResponseValues(device, tapEnv, initialWiFiRadioValues,
							wifiRadioValues);
					successMessage = "Sucessfully cross verified the 2.4 & 5 GHz Radios values with initial 2.4 & 5 GHz Radios values.";
				} else {
					LOGGER.info("Verifying the wifiRadioValues are empty or null");
					status = BroadBandWebPaUtils.getAndVerifyMapValueIsNotNullOrEmpty(wifiRadioValues);
				}
			}
		} catch (Exception e) {
			errorMessage += e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : " + successMessage);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	}

	/**
	 * Test method used to get and verify the common parameter values
	 * 
	 * @param device                   Instance of {@link Dut}
	 * @param testId                   Test case ID
	 * @param stepNumber               Step Number
	 * @param initialCommonParamValues Key and pair value mapping
	 * @param isCompare                True- Compare with initial values
	 * @throws TestException
	 * @refactor Govardhan
	 */
	public static void executeTestSeptsToGetAndVerifyCommonParameterValues(Dut device, String testId, int stepNumber,
			Map<String, String> initialCommonParamValues, boolean isCompare) throws TestException {
		String step = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String successMessage = null;
		Map<String, String> commonParamValues = null;
		/**
		 * Step : RETRIEVE THE COMMON PARAMETER VALUES FROM DEVICE
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Retrieve common parameter values from device");
		LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa Command :\n "
				+ "	1. Get the value for DMZ LAN IP Address \n" + "	2. Get the value for Device cloud UI status \n"
				+ "	3. Get the value for device control router enable status \n"
				+ "	4. Get the value for port forwarding status");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must return the common parameter values from device.");
		LOGGER.info("**********************************************************************************");
		errorMessage = "Unable to retrieve the common parameter values from device.";
		try {
			commonParamValues = BroadBandWebPaUtils.executeAndGetListOfWebParameters(device, tapEnv,
					DeviceModeHandler.isBusinessClassDevice(device)
							? BroadBandWebPaConstants.WEBPA_PARAMS_FOR_COMMON_BUSI_DEVICE
							: BroadBandWebPaConstants.WEBPA_PARAMS_FOR_COMMON_RESI_DEVICE);
			status = !commonParamValues.isEmpty();
			successMessage = "Sucessfully retrieved the common parameter values from device";
			if (status) {
				status = false;
				if (isCompare) {
					LOGGER.info("Comparing the commonParamValues with initial values");
					errorMessage = "Unable to cross verify the common parameter values with initial common parameter values.";
					status = BroadBandWebPaUtils.verifyWebpaGetResponseValues(device, tapEnv, initialCommonParamValues,
							commonParamValues);
					successMessage = "Sucessfully cross verified the common parameter values with initial common parameter values.";
				} else {
					LOGGER.info("Verifying the commonParamValues are empty or null");
					status = BroadBandWebPaUtils.getAndVerifyMapValueIsNotNullOrEmpty(commonParamValues);
				}
			}
		} catch (Exception e) {
			errorMessage += e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : " + successMessage);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	}

	/**
	 * Test method used to enable the bridge mode
	 * 
	 * @param device     Instance of {@link Dut}
	 * @param testId     Test case ID
	 * @param stepNumber Step Number
	 * @throws TestException
	 * @refactor Govardhan
	 */
	public static boolean executeTestSeptsToEnableBridgeMode(Dut device, String testId, int stepNumber)
			throws TestException {
		String step = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Enable Bridge mode in device");
		LOGGER.info("STEP " + stepNumber + " : ACTION : Execute Command: "
				+ BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE);
		LOGGER.info("STEP " + stepNumber
				+ " : EXPECTED : Device should return the current mode and the value should be \"bridge-static\"");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
					BroadBandTestConstants.LAN_MANAGEMENT_MODE_BRIDGE_STATIC,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				status = false;
				LOGGER.info("Mode changed to " + BroadBandTestConstants.LAN_MANAGEMENT_MODE_BRIDGE_STATIC);
				LOGGER.info("Waiting for three minutes");
				tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
				status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			}
		} catch (Exception e) {
			errorMessage += e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully verified bridge mode status using webpa");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
		return status;
	}

	/**
	 * Method to verify factory reset via different method (Webpa, TR69 & SNMP)
	 * 
	 * @param device           Dut instance
	 * @param testCaseId       testcase Id
	 * @param factoryResetType Factory reset method to be done through
	 *                         WebPA/TR69/SNMP
	 */
	public void testToVerifyFactoryReset(Dut device, String testCaseId, String factoryResetType) {
		// Variable declaration starts
		int stepNum = 1;
		int preConditionNumber = 0;
		int uptimeInMin = 0;
		String stepNumber = "s" + stepNum;
		String errorMessage = null;
		String response = null;
		boolean status = false;
		// Variable declaration ends
		try {

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: " + testCaseId);
			LOGGER.info("TEST DESCRIPTION: Verifyy performing Factory Reset Using " + factoryResetType);
			LOGGER.info("#######################################################################################");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			preConditionNumber++;

			try {
				errorMessage = "Unable to execute sysevent getwan-status command.";
				LOGGER.info("#######################################################################################");
				LOGGER.info(
						"PRE-CONDITION " + preConditionNumber + " : DESCRIPTION : Verify WAN status through sysevent.");
				LOGGER.info("PRE-CONDITION " + preConditionNumber
						+ ": ACTION : SSH the device & Execute the following command immediately after reboot: \\n\" + \"sysevent getwan-status");
				LOGGER.info("PRE-CONDITION " + preConditionNumber + ": EXPECTED :WAN status should be retrieved.");
				LOGGER.info("#######################################################################################");
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.GET_WAN_STATUS);
				status = CommonMethods.isNotNull(response) && !response.contains(BroadBandTestConstants.CMD_NOT_FOUND);
			} catch (Exception exception) {
				LOGGER.error("Exception occured while verifying WAN status through sysevent.");
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION " + preConditionNumber + " : ACTUAL : WAN status retrieved as follows- "
						+ response);
			} else {
				LOGGER.error("PRE-CONDITION " + preConditionNumber + " : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + preConditionNumber + ": FAILED : " + errorMessage);
			}

			// Map containing default values for factory reset setting option.
			Map<FactoryResetSettings, String> defaultValues = getDefaultValuesInMap(device, preConditionNumber);
			// List containing factory reset settings option for which value is
			// not successfully.
			List<FactoryResetSettings> skipList = new ArrayList<FactoryResetSettings>();

			// Adding Parameter to Skip list if unable to get default value for
			// that parameter
			for (Map.Entry<FactoryResetSettings, String> entry : defaultValues.entrySet()) {
				if (CommonMethods.isNull(entry.getValue())
						|| CommonMethods.patternMatcher((entry.getValue()),
								BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
						|| CommonMethods.patternMatcher((entry.getValue()),
								BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID)
						|| CommonMethods.patternMatcher((entry.getValue()),
								BroadBandSnmpConstants.SNMP_ERROR_NON_WRITABLE)) {
					skipList.add(entry.getKey());
					LOGGER.info("Default Value is not obtained for parameter " + entry.getKey()
							+ " so adding parameter to skip list.");

				}
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/** Step 1: Verifying whether device uptime is more than 20 min */
			errorMessage = "Unable to get device Uptime.";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify device uptime is more than 20 minutes.");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Execute the Webpa Get command for param: Device.DeviceInfo.UpTime");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Uptime should be more than 20 minutes.");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_UPTIME);
			if (CommonMethods.isNotNull(response)) {
				uptimeInMin = Integer.parseInt(response) / BroadBandTestConstants.CONSTANT_60;
				if (uptimeInMin > BroadBandTestConstants.CONSTANT_20) {
					LOGGER.info("Device uptime is more than 20 min.");
				} else {
					int WaitTime = BroadBandTestConstants.CONSTANT_20 - uptimeInMin;
					LOGGER.error("Device uptime is " + uptimeInMin + ". Will wait for " + WaitTime + " min.");
					tapEnv.waitTill(WaitTime * 60000);
				}
				status = true;
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Device uptime should be more than 20 minutes.");
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 2 : Verify setting SSID Name value other than default SSID Name of
			 * 2.4Ghz and 5 Ghz using WebPA request.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify setting SSID Name value other than default SSID Name of 2.4Ghz and 5 Ghz using WebPA request.");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION :Execute WebPA set command  for following param: 'Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID' & 'Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID'.");
			LOGGER.info("STEP " + stepNumber + " : WebPa Set Command should return success message.");
			LOGGER.info("**********************************************************************************");
			FactoryResetSettings[] factoryResetSettings = WifiSsid.values();
			errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
					defaultValues, skipList, tapEnv);
			status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified setting SSID name value other than default value.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 3 : Modify Parental Control service value other than default value
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify modifying Parental Control service value other than default value.");
			LOGGER.info("STEP " + stepNumber + " : ACTION :Execute the WebPA set command for following param: "
					+ "Parameter and Value : \\n" + "1. Parental Control Managed Sites \\n"
					+ "Name : Device.X_Comcast_com_ParentalControl.ManagedSites.Enable, value: true, dataType:3 \\n"
					+ "2. Parental Control Managed Service \\n"
					+ "Name :Device.X_Comcast_com_ParentalControl.ManagedServices.Enable, value : true, dataType:3 \\n"
					+ "3. Parental Control Managed Devices \\n"
					+ "Name : Device.X_Comcast_com_ParentalControl.ManagedDevices.Enable, Value:true, dataType:3");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : Webpa Command should return success message ");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = ParentalControlService.values();
			errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
					defaultValues, skipList, tapEnv);
			status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully modified parental control value other than default values.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 4 : Modify Advanced feature service value other than default value.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify Modifying Advanced feature service value other than default value.");
			LOGGER.info("STEP " + stepNumber + " : ACTION :Execute the WebPA set command for following params:"
					+ "Parameter and Value : \\n" + "1. Port Mapping \\n"
					+ "Name : Device.NAT.X_Comcast_com_EnablePortMapping, value: true, dataType:3 \\n"
					+ "2. HS  Port Mapping -  \\n"
					+ "Name : Device.NAT.X_Comcast_com_EnableHSPortMapping, value : false, dataType:3 \\n"
					+ "3. Port Trigger \\n"
					+ "Name : Device.NAT.X_CISCO_COM_PortTriggers.Enable, value:true, dataType:3 \\n"
					+ "4.Remote Management \\n"
					+ "Name : Device.UserInterface.X_CISCO_COM_RemoteAccess.Enable, value : true, dataType:3 \\n"
					+ "5. DMZ \\n" + "Name : Device.NAT.X_CISCO_COM_DMZ.Enable, value:true, dataType:3 \\n"
					+ "6.Routing > Authentication Type \\n"
					+ "Name : Device.Routing.RIP.InterfaceSetting.1.X_CISCO_COM_AuthenticationType,value: \"SimplePassword\", dataType:0 \\n"
					+ "7.Dynamic DNS \\n" + "Name : Device.X_CISCO_COM_DDNS.Enable, value: true, dataType:3 \\n"
					+ "8.Device Discovery > upnp \\n" + "Name : Device.UPnP.Device.Enable, value : false, dataType:3 ");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = AdvancedFeatureServices.values();
			errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
					defaultValues, skipList, tapEnv);
			status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully modified Advanced feature service value other than default values.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 5 : Modify Local Gateway IPV4, DHCP start and end value.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verfiy Modifying Local Gateway IPV4, DHCP start and end value other than default value.");
			LOGGER.info("STEP " + stepNumber + " : ACTION :Execute the WebPA set command for following params: "
					+ "Parameter and Value : \\n" + "1. LOCAL GATEWAY IP \\n"
					+ "Name : Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanIPAddress, value: 10.0.10.1, dataType:0 \\n"
					+ "2. LOCAL DHCP start address \\n"
					+ "Name :Device.DHCPv4.Server.Pool.1.MinAddress, value : 10.0.10.12, dataType:0 \\n"
					+ "3. LOCAL DHCP end address \\n"
					+ "Name : Device.DHCPv4.Server.Pool.1.MaxAddress, Value: 10.0.10.220, dataType:0");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPa Command should return success message.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = LocalGatewayIPv4.values();
			errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
					defaultValues, skipList, tapEnv);
			status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully modified local gateway IPV4,DHCP start ,DHCP end value other than default values");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 6 :Verify Changing 2.4 GHz channel bandwidth to value other than 20/40
			 * MHz.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Verify Changing 2.4 GHz channel bandwidth to value other than 20/40 MHz.");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION :Execute the WebPa Get command for following param: Device.WiFi.Radio.10000.OperatingChannelBandwidth and set value as 20.");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = WifiSettings2Ghz.values();
			errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
					defaultValues, skipList, tapEnv);
			status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified changing 2.4GHz channel bandwidth to value other than default value.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 8 : Enable Code big using WebPA request.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION :  Verifying Enabling Code Big through WebPA request.");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Execute the Webpa Set command for following param : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable and set value as 'true'.");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = CodeBig.values();
			errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
					defaultValues, skipList, tapEnv);
			status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified changing Code Big enable to value other than default value");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 9 : Enable Telemetry Endpoint through WebPA request
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION :  Verify Enabling Telemetry Endpoint through WebPA request.");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Execute the WebPa set command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable & set value as 'true'.");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = TelemetryEndpoint.values();
			errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
					defaultValues, skipList, tapEnv);
			status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified changing Telemetry Endpoint Enable to value other than default value.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 10 : set LimitBeaconDetectionEnum to true.
			 */

			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify Setting LimitBeaconDetection to true.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute the WebPa Set command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection and set value as true.");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPa Command should return success message.");
			LOGGER.info("**********************************************************************************");

			Boolean isSupportedDevices = null;

			isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

			if (isSupportedDevices) {
				factoryResetSettings = LimitBeaconDetectionEnum.values();
				errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
						defaultValues, skipList, tapEnv);
				status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
				if (status) {
					LOGGER.info("STEP " + stepNumber + " : ACTUAL : LimitBeaconDetection is set to true.");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * Step 11 :Verify Changing rabid framework memory limit to 10MB.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + " : DESCRIPTION : Verify Changing rabid framework memory limit to 10MB.");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION :Execute the Webpa Set command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.MemoryLimit & set value as '10'.");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
			LOGGER.info("**********************************************************************************");
			// TR-181 to configure "MemoryLimit" for Rabid is not applicable for Business
			// class devices
			if (DeviceModeHandler.isDSLDevice(device) || DeviceModeHandler.isBusinessClassDevice(device)
					|| DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("This Step is not applicable for DSL, Business class devices and RPi devices");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						"Step is not applicable for DSL, Business class devices and RPi devices", false);
			} else {
				factoryResetSettings = RabidMemoryLimit.values();
				errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
						defaultValues, skipList, tapEnv);
				status = !errorMessage.toString().contains(BroadBandTestConstants.STRING_FAILED);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Successfully verified changing rabid framework memory limit to value other than default value");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			}

			/**
			 * Step 12 : Set Force WiFi Disable to true
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify setting Force WiFi Disable to true.");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : Execute the Webpa Set command for following param: Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable & set value as 'true'.");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPa Command should return success message.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = ForceWiFiDisable.values();
			errorMessage = BroadBandFactoryResetUtils.updateSettingsOnDevice(device, factoryResetSettings,
					defaultValues, skipList, tapEnv);
			status = !CommonUtils.isGivenStringAvailableInCommandOutput(errorMessage,
					BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified changing force wifi disable to value other than default value.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 13 : Perform Factory reset on the device using SNMP/WEBPA/TR-69 request.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify Performing Factory reset on the device using "
					+ factoryResetType + " request.");
			switch (factoryResetType) {
			case TestCategory.WEBPA:
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber + ": ACTION : Execute below command -  "
						+ "curl -i -H \"Authorization:Bearer <SAT_TOKEN>\" -X PATCH -H \"Content-Type:application/json\" -H \"Accept:application/json\" -w %{time_total} -k \"https://<url:port>/api/v2/device/mac:<ECM_MAC>/config\" -d '{\"parameters\":[{\"dataType\":0,\"name\":\"Device.X_CISCO_COM_DeviceControl.FactoryReset\":\"Router,Wifi,VoIP,Dect,MoCA\"}]}'");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : WebPA Command should return success message and log message \"Received reboot_reason as:factory-reset\" should be present in /rdklogs/logs/WEBPAlog.txt.0 file.");
				LOGGER.info("**********************************************************************************");
				status = BroadBandFactoryResetUtils.performFactoryUsingWebpaAndVerifyInBootTimeLog(device, tapEnv);
				errorMessage = "Failed to factory reset wifi interface using WebPa parameter "
						+ WebPaParamConstants.WEBPA_PARAM_FACTORY_RESET;
				break;
			case TestCategory.TR69:
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber + ": ACTION : Execute below TR069 command -  "
						+ "curl --tlsv1.2 --request POST --url https://<url>/telescope/api/<model>/<serialnumber>/ --header ': ' --header 'content-type: application/json' --data '{\"parameters\":[{\"dataType\":0,\"name\":\"Device.X_CISCO_COM_DeviceControl.FactoryReset\",\"value\":\"Router,Wifi,VoIP,Dect,MoCA\"}]}'");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : SNMP Command should successfully get executed and log message \"Received reboot_reason as:factory-reset\" should be present in /rdklogs/logs/WEBPAlog.txt.0 file.");
				LOGGER.info("**********************************************************************************");
				status = BroadBandFactoryResetUtils.performFactoryResetTR69(tapEnv, device);
				errorMessage = "Failed to factory reset using TR69 with parameter "
						+ WebPaParamConstants.WEBPA_PARAM_FACTORY_RESET;
				if (status) {
					BroadBandTr69Utils.enableOrVerifyTheTr69Configuration(device, tapEnv);
				}
				break;
			case TestCategory.SNMP:
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber + ": ACTION : Execute below command -  "
						+ "snmpset -OQ -v 2c -c <community string> udp6:[ecm_ip]:161 1.3.6.1.4.1.17270.50.2.1.1.1002.0 i 1");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : WebPA Command should return success message and log message \"Received reboot_reason as:factory-reset\" should be present in /rdklogs/logs/WEBPAlog.txt.0 file.");
				LOGGER.info("**********************************************************************************");
				status = BroadBandCommonUtils.performFactoryUsingSNMPAndVerifyInBootTimeLog(device, tapEnv);
				if (status) {
					LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully performed factory reset via "
							+ factoryResetType);
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				errorMessage = "Failed to factory reset wifi interface using snmp OID "
						+ BroadBandSnmpMib.FACTORY_RESET_DEVICE.getOid();
				break;
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 14 : Verify the Current SSID value for 2.4 Ghz using WebPA request..
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :  Verify the Current SSID value for 2.4Ghz and 5 Ghz using WebPA request.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute the Webpa Get command for following param: 'Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID' & 'Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID'.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  Current SSID value should be the same as the SSID value from step1.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = WifiSsid.values();
			errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
			status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified current SSID value for 2.4GHz is default after factory reset.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 15 : Verify Parental Control service value with the default value
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Parental Control service value with the default value.");
			LOGGER.info("STEP " + stepNumber + ": ACTION :Execute the WebPA get command with the below configuration"
					+ "Parameter and Value : \\n" + "1. Parental Control Managed Sites \\n"
					+ "Name : Device.X_Comcast_com_ParentalControl.ManagedSites.Enable \\n"
					+ "2. Parental Control Managed Service \\n"
					+ "Name :Device.X_Comcast_com_ParentalControl.ManagedServices.Enable \\n"
					+ "3. Parental Control Managed Devices \\n"
					+ "Name : Device.X_Comcast_com_ParentalControl.ManagedDevices.Enable");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Default value and current value of the Parental Control Services should be the same.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = ParentalControlService.values();
			errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
			status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified parental control value is default after factory reset.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 16 : Verify Advanced feature service value with default value.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Verify Advanced feature service value with default value.");
			LOGGER.info("STEP " + stepNumber + ": ACTION :Execute the WebPA get command with the below configuration"
					+ "Parameter \\n" + "1. Port Mapping \\n" + "Name : Device.NAT.X_Comcast_com_EnablePortMapping,\\n"
					+ "2. HS  Port Mapping -  \\n" + "Name : Device.NAT.X_Comcast_com_EnableHSPortMapping,\\n"
					+ "3. Port Trigger \\n" + "Name : Device.NAT.X_CISCO_COM_PortTriggers.Enable,\\n"
					+ "4.Remote Management \\n" + "Name : Device.UserInterface.X_CISCO_COM_RemoteAccess.Enable,\\n"
					+ "5. DMZ \\n" + "Name : Device.NAT.X_CISCO_COM_DMZ.Enable,\\n"
					+ "6.Routing > Authentication Type \\n"
					+ "Name : Device.Routing.RIP.InterfaceSetting.1.X_CISCO_COM_AuthenticationType,\\n"
					+ "7.Dynamic DNS \\n" + "Name : Device.X_CISCO_COM_DDNS.Enable,\\n"
					+ "8.Device Discovery > upnp \\n" + "Name : Device.UPnP.Device.Enable,");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  Default value and current value of the  Advanced feature Services should be the same.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = AdvancedFeatureServices.values();
			errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
			status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified Advanced feature service is default after factory reset.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 17 : Verify Local Gateway IPV4, DHCP start and end value with default
			 * value.
			 */

			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Local Gateway IPV4, DHCP start and end value with default value.");
			LOGGER.info("STEP " + stepNumber + ": ACTION :Execute the WebPA set command with the below configuration"
					+ "Parameter and Value : \\n" + "1. LOCAL GATEWAY IP \\n"
					+ "Name : Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanIPAddress, \\n"
					+ "2. LOCAL DHCP start address \\n" + "Name :Device.DHCPv4.Server.Pool.1.MinAddress, \\n"
					+ "3. LOCAL DHCP end address \\n" + "Name : Device.DHCPv4.Server.Pool.1.MaxAddress");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Default value and current value of the  Local Gateway IPV4, DHCP start and end value should be the same.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = LocalGatewayIPv4.values();
			errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
			status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified Local Gateway IPV4,DHCP start and DHCP end value is default after factory reset.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 18 : Verify the Current SSID value for 2.4 Ghz using SNMP request.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :  Verify the Current SSID value for 2.4Ghz and 5 Ghz using SNMP request.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION :Execute the SNMP Get command for 2.4 Ghz and 5Ghz using SNMP OID.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  Current SSID value should be the same as the SSID value from step1.");
			LOGGER.info("**********************************************************************************");
			if (!DeviceModeHandler.isDSLDevice(device)) {
				factoryResetSettings = WifiSsidSnmp.values();
				errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
				status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Successfully verified Local Gateway IPV4,DHCP start and DHCP end value is default after factory reset.");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * Step 19 : Verify 2.4 GHz channel bandwidth value is 20/40 MHz.
			 */
			stepNumber = "s" + (++stepNum);
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify 2.4 GHz channel bandwidth value is 20/40 MHz.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Get channel bandwidth value using webpa for the parameter Device.WiFi.Radio.10000.OperatingChannelBandwidth.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  WebPA response should be successful and value should be 40Mhz.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = WifiSettings2Ghz.values();
			errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
			status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully verified 2.4GHZ channel bandwidth value is default after factory reset");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 21 : Verify that the CodeBigFirst Enable has default value
			 */
			stepNumber = "s" + ++stepNum;
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify that the CodeBigFirst Enable has default value.");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute following dmcli command: dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Successfully verified default value of CodeBig Enable.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = CodeBig.values();
			errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
			status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL : Successfully verified default value of CodeBig Enable.");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 22 : Verify that the TelemetryEndpoint Enable has default value
			 */
			stepNumber = "s" + ++stepNum;
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNum + ": DESCRIPTION : Verify that the TelemetryEndpoint Enable has default value.");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute following dmcli command: dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED : Successfully verified default value of TelemetryEndpoint Enable.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = TelemetryEndpoint.values();
			errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
			status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);

			// if telemetry endpoint is enabled via RFC.Then Default value of
			// TELEMETRY_ENDPOINT_ENABLE can be retrived from /rdklogs/logs/dcmrfc.log
			if (!status) {
				String defaultValue = TelemetryEndpoint.TELEMETRY_ENDPOINT_ENABLE.getDefaultValue(tapEnv, device);
				String searchValue = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTestConstants.PATTERN_TO_GREP_TELEMETRY_ENDPOINT_ENABLE,
						BroadBandTestConstants.DCMRFC_LOG_FILE, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (CommonMethods.isNotNull(searchValue) && CommonMethods.isNotNull(defaultValue)) {
					searchValue = CommonMethods.patternFinder(searchValue,
							BroadBandTestConstants.PATTERN_TO_FIND_TELEMETRY_ENDPOINT_ENABLE);
				}
				status = CommonMethods.isNotNull(searchValue)
						&& CommonMethods.patternMatcher(defaultValue, searchValue);
			}

			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL : Successfully verified default value of TelemetryEndpoint Enable.");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 23 : Verify the default value of
			 * Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection is FALSE
			 * after Factory Reset. This step is only for specific devices
			 */
			stepNumber = "s" + ++stepNum;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION : Verify the default value of LimitBeaconDetection is false after Factory Reset.");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute the Webpa Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED : Default value of LimitBeaconDetection should be retrieved as false.");
			LOGGER.info("**********************************************************************************");

			isSupportedDevices = null;

			isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

			if (isSupportedDevices) {
				factoryResetSettings = LimitBeaconDetectionEnum.values();
				errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
				status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ " : ACTUAL : Default value  of LimitBeaconDetection is retrieved as FALSE after factory reset.");
				} else {
					LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * Step 24 : Verify that the rabid framework memory limit has default value
			 */
			stepNumber = "s" + ++stepNum;
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION : Verify that the Rabid framework memory limit has default value.");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute following dmcli command: dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.MemoryLimit");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED : Default value of rabid framework memory limit should be verified.");
			LOGGER.info("**********************************************************************************");

			// TR-181 to configure "MemoryLimit" for Rabid is not applicable for Business
			// class devices
			if (DeviceModeHandler.isDSLDevice(device) || DeviceModeHandler.isBusinessClassDevice(device)
					|| DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("This Step is not applicable for DSL, Business class devices and RPi devices");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						"Step is not applicable for DSL, Business class devices and RPi devices", false);
			} else {
				factoryResetSettings = RabidMemoryLimit.values();
				errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
				status = !errorMessage.contains(BroadBandTestConstants.STRING_FAILED);
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL : Successfully verified default value of Rabid framework memory limit");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
				}
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			}

			/**
			 * Step 25 : Verify that Force WiFi Disable has default value
			 */
			stepNumber = "s" + ++stepNum;
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify that Force WiFi Disable has default value.");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute the Webpa Get command for following param: Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Default value of force wifi disable should verified.");
			LOGGER.info("**********************************************************************************");
			factoryResetSettings = ForceWiFiDisable.values();
			errorMessage = validateSettingsOnDevice(device, factoryResetSettings, defaultValues, skipList);
			status = !CommonUtils.isGivenStringAvailableInCommandOutput(errorMessage,
					BroadBandTestConstants.STRING_FAILED);
			if (status) {
				LOGGER.info(
						"STEP " + stepNum + ": ACTUAL : Successfully verified default value of force wifi disable.");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING FACTORY REBOOT THROUGH " + factoryResetType + " : "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("Checking whether device is Accessible");
			status = CommonMethods.isSTBAccessible(device);
			if (!status) {
				status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
			}
			if (status) {
				status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				if (status) {
					BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
				} else {
					LOGGER.error("Could not start the reactivation sequence"
							+ " since webpa is not UP for the device after factory reboot");
				}
			} else {
				LOGGER.error(
						"Failed to reactivate the Broadband Device using WebPA Parameters, as device is not accessible.");
			}
			LOGGER.info("################### ENDING POST-CONFIGURATIONS ###################");
		}
	}

	/**
	 * Helper method to get default values of all setting in MAP
	 * 
	 * @param device Dut instance
	 * @return default value of settings with key and pair
	 * @author Praveenkumar Paneerselvam
	 * @Refactor Athira
	 */
	private Map<FactoryResetSettings, String> getDefaultValuesInMap(Dut device, int preConditionNumber) {
// Variable declaration starts
// Map containing default values for factory reset setting option.
		Map<FactoryResetSettings, String> defaultValues = new HashMap<FactoryResetSettings, String>();
		String errorMessage = null;
		boolean status = false;
		String response = null;
// Variable declaration ends
//PC2
		/**
		 * Pre-Condition : Getting default value for 2.4GHz & 5GHz SSID via WebPa.
		 */
		preConditionNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ ": DESCRIPTION : Verify getting default SSID values for 2.4GHz and 5GHz using WebPA request.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : ACTION  :Execute the WebPA Get command for following params: 'Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID' & 'Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID'");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : EXPECTED  : WebPA Get request should return default SSID value with success message.");
		LOGGER.info("#######################################################################################");
// Pre- Condition to Get default values for WiFi SSID via WebPa
		FactoryResetSettings[] factoryResetSettings = WifiSsid.values();
		BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		LOGGER.info("#######################################################################################");

//PC3
		/**
		 * Pre-Condition : Getting default value for 2.4GHz & 5GHz SSID via SNMP.
		 */
		if (!DeviceModeHandler.isDSLDevice(device)) {
			preConditionNumber++;
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ ": DESCRIPTION : Verify getting default SSID values for both 2.4GHz & 5GHz via SNMP.");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ ": ACTION :Excecute SNMP Get command for following OID for both SSIDs: 1.3.6.1.4.1.17270.50.2.2.2.1.1.16.");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ " : EXPECTED : SNMP Get request should be successful and return default value for both SSIDs.");
			LOGGER.info("#######################################################################################");
			// Pre- Condition to get default values for Wifi SSID via SNMP
			factoryResetSettings = WifiSsidSnmp.values();
			BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
			LOGGER.info("#######################################################################################");
		}
//PC4
		/**
		 * Pre-Condition : Getting default values for Parental Control Services
		 */
		preConditionNumber++;
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : DESCRIPTION : Verify getting default values for Parental Control Services.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : ACTION : SSH the device & Execute following commands: \\n" + "1. Managed Sites  \\n"
				+ "grep -i managedsites_enabled= /etc/utopia/system_defaults  \\n" + "2. Managed Services - \\n"
				+ "grep -i managedservices_enabled= /etc/utopia/system_defaults \\n" + "3. Managed Devices \\n"
				+ "grep -i manageddevices_enabled= /etc/utopia/system_defaults \\n");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : EXPECTED : Default value for all Parental Control parameter should be obtained successfully.");
		LOGGER.info("#######################################################################################");
// Pre-Condition to Get default values for Parental Control Services
		factoryResetSettings = ParentalControlService.values();
		BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		LOGGER.info("#######################################################################################");
//PC5
		/**
		 * Pre-Condition : Getting default values for Advanced Features Services
		 */
		preConditionNumber++;
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : DESCRIPTION : Verify getting default values for Advanced feature Services.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : ACTION : SSH the device & Execute following commands: \\n" + "1. Port Mapping  \\n"
				+ "grep -i port_forward_enabled=  /etc/utopia/system_defaults  \\n" + "2. HS  Port Mapping - \\n"
				+ "grep -i port_hs_forward_enabled=  /etc/utopia/system_defaults \\n" + "3. Port Trigger \\n"
				+ "grep -i port_trigger_enabled=  /etc/utopia/system_defaults \\n" + "5. DMZ\\n"
				+ "grep -i dmz_enabled= /etc/utopia/system_defaults \\n" + "7.Dynamic DNS\\n"
				+ "grep -i ddns_enable= /etc/utopia/system_defaults\\n" + "8.Device Discovery > upnp\\n"
				+ "grep -i upnp_igd_enabled= /etc/utopia/system_defaults");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : EXPECTED : Default value for all Advanced feature services parameter should be obtained successfully.");
		LOGGER.info("#######################################################################################");
// Pre-Condition to Get default values for Advanced Feature Services
		factoryResetSettings = AdvancedFeatureServices.values();
		BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		LOGGER.info("#######################################################################################");
//PC6
		/**
		 * Pre-Condition : Getting Local Gateway IPv4, DHCP LOCAL IPv4 Start, DHCP LOCAL
		 * IPv4 End
		 */
		preConditionNumber++;
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : DESCRIPTION : Verify getting default values for LOCAL Gateway IPv4, DHCP LOCAL IPv4 Start, DHCP LOCAL IPv4 End.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : ACTION : Execute below commands to get default value for Advanced feature services \\n"
				+ "1. Local Gateway IP  from " + BroadBandWebPaConstants.WEBPA_PARAM_DEFAULT_LANIP_FOR_SYNDICATION
				+ " /nvram/partners_defaults.json  \\n" + "2. Local IP DHCP start from "
				+ BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS
				+ "  /nvram/partners_defaults.json \\n" + "3. Local IP DHCP end from"
				+ BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS
				+ "  /nvram/partners_defaults.json \\n");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : EXPECTED : Default value for all parameter should be obtained successfully. ");
		LOGGER.info("#######################################################################################");
// Pre- Condition to get default values for LOCAL Gateway IPv4, DHCP LOCAL
// IPv4 Start, DHCP LOCAL IPv4 End
		response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
		factoryResetSettings = LocalGatewayIPv4.values();

		status = compareDhcpDefaultValues(tapEnv, device);
		if (status) {
			BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		} else {
			errorMessage = " LocalGatewayIP,DHCP start,DHCP end value in default file and expected default value are not same ";
			throw new TestException(
					BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION  : FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");

//PC6
		/**
		 * Pre-Condition : Getting default value for 2.4GHz Channel bandwidth.
		 */

		preConditionNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : DESCRIPTION : Verify getting default value for 2.4GHz Channel bandwidth.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : ACTION :Execute the WebPA Get command for following param: Device.WiFi.Radio.10000.OperatingChannelBandwidth");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ ": EXPECTED : Default value for 2.4GHz channel bandwidth should be set to 40MHz.");
		LOGGER.info("#######################################################################################");
// Default value is hardcoded as 40MHz
		factoryResetSettings = WifiSettings2Ghz.values();
		BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		LOGGER.info("#######################################################################################");

		/**
		 * Pre-Condition : Getting default value for CodeBig Enable
		 */
		preConditionNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : DESCRIPTION : Verify CodeBig Enable is disabled by default.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : ACTION : Execute the WebPA Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable");
		LOGGER.info("PRE-CONDITION " + preConditionNumber + " : EXPECTED : Code Big should be disabled by default.");
		LOGGER.info("#######################################################################################");
// Default value is false
		factoryResetSettings = CodeBig.values();
		BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		LOGGER.info("#######################################################################################");

		/**
		 * Pre-Condition : Getting default value for Telemetry Endpoint
		 */
		preConditionNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : DESCRIPTION : Verify Telemetry Endpoint is disabled by default.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : ACTION :Execute the WebPA Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.TelemetryEndpoint.Enable");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : EXPECTED : Telemetry Endpoint should be disabled by default.");
		LOGGER.info("#######################################################################################");
// Default value is false 
		factoryResetSettings = TelemetryEndpoint.values();
		BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		LOGGER.info("#######################################################################################");

		/**
		 * Getting default value for LimitBeaconDetection
		 */

		Boolean isLimitBeaconDevice = null;
		isLimitBeaconDevice = BroadbandPropertyFileHandler.isDeviceLimitBeacon(device);

		if (isLimitBeaconDevice) {
			preConditionNumber++;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ " DESCRIPTION:  Verify getting default value for LimitBeaconDetection.");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ " ACTION : Execute WebPa Get comamnd for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ " EXPTECTED: LimitBeaconDetection should be false by default.");
			LOGGER.info("*********************************************************************************");

			// Defaulf value of LimitBeaconDetectionEnum is false
			factoryResetSettings = LimitBeaconDetectionEnum.values();
			BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		}
		LOGGER.info("#######################################################################################");
		/**
		 * Pre-Condition : Getting Rabid Framework memory limit
		 */
		Boolean isRabidFrameworkDevice = null;
		isRabidFrameworkDevice = !DeviceModeHandler.isRPIDevice(device);

		if (isRabidFrameworkDevice) {
			preConditionNumber++;
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ " : DESCRIPTION : Verify getting default value for rabid framework memory limit.");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ " : ACTION :Execute WebPA Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.MemoryLimit");
			LOGGER.info("PRE-CONDITION " + preConditionNumber
					+ " : EXPECTED : Rabid framework memory limit should be 20MB by default.");
			LOGGER.info("#######################################################################################");
// Default value is false 
			factoryResetSettings = RabidMemoryLimit.values();
			BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		}
		LOGGER.info("#######################################################################################");
// Default value is false 
		factoryResetSettings = RabidMemoryLimit.values();
		BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		LOGGER.info("#######################################################################################");

		/**
		 * Pre-Condition : Getting Force WiFi default value
		 */
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : DESCRIPTION : Verify force wifi disable is disabled by default.");
		LOGGER.info("PRE-CONDITION " + preConditionNumber
				+ " : ACTION :Execute WebPA Get command for following param:  Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable");
		LOGGER.info(
				"PRE-CONDITION " + preConditionNumber + " : EXPECTED : Force wifi disable should be false by default.");
		LOGGER.info("#######################################################################################");
// Default value is false 
		factoryResetSettings = ForceWiFiDisable.values();
		BroadBandFactoryResetUtils.addDefaultValueInMap(device, tapEnv, defaultValues, factoryResetSettings);
		LOGGER.info("#######################################################################################");

		return defaultValues;

	}

	/**
	 * Helper method to compare Lan IP,DHCP start,DHCP end value with default values
	 * 
	 * @param device instance of {@link Dut}
	 * @param tapEnv instance of {@link AutomaticsTapApi}
	 * @return true if default and obtained value is same false-if they are
	 *         different
	 */
	public static boolean compareDhcpDefaultValues(AutomaticsTapApi tapEnv, Dut device) {
		boolean status = false;
		String response = null;
		boolean isLanIp = false;
		boolean isDhcpStart = false;
		boolean isDhcpEnd = false;
		try {
			response = (DeviceModeHandler.isBusinessClassDevice(device)) ? BroadBandTestConstants.COMMERCIAL
					: tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);

			if (CommonMethods.isNotNull(response)) {
				isLanIp = CommonMethods.isNotNull(
						BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_GATEWAY_IP.getDefaultValue(tapEnv, device))
						&& CommonUtils
								.patternSearchFromTargetString(
										BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_GATEWAY_IP
												.getDefaultValue(tapEnv, device),
										BroadBandCommonUtils.getSynLocalIPforPartner(response));

				isDhcpStart = CommonMethods.isNotNull(
						BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_DHCP_START.getDefaultValue(tapEnv, device))
						&& CommonUtils
								.patternSearchFromTargetString(
										BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_DHCP_START
												.getDefaultValue(tapEnv, device),
										BroadBandCommonUtils.getSynStartIPforPartner(response));

				isDhcpEnd = CommonMethods.isNotNull(
						BroadBandFactoryResetUtils.LocalGatewayIPv4.LAN_DHCP_END.getDefaultValue(tapEnv, device))
						&& CommonUtils.patternSearchFromTargetString(
								LocalGatewayIPv4.LAN_DHCP_END.getDefaultValue(tapEnv, device),
								BroadBandCommonUtils.getSynEndIPforPartner(response));
				status = isLanIp && isDhcpStart && isDhcpEnd;
			}
		} catch (Exception exception) {
			LOGGER.error("Exception occurred while validating LocalGatewayIp,DHCP start value,DHCP end value"
					+ exception.getMessage());
		}
		return status;
	}

	/**
	 * Method to validate set value with default value on device
	 * 
	 * @param device               Dut Instance
	 * @param factoryResetSettings factory reset setting option for which validation
	 *                             comparison has to be done
	 * @param defaultValues        default value for the particular setting.
	 * @param skipList             has settings value for which new value is not
	 *                             set.
	 * @return error or success message.
	 * @author Praveenkumar Paneerselvam
	 * @Refactor Athira
	 */
	private String validateSettingsOnDevice(Dut device, FactoryResetSettings[] factoryResetSettings,
			Map<FactoryResetSettings, String> defaultValues, List<FactoryResetSettings> skipList) {
		StringBuilder message = new StringBuilder();
		LOGGER.debug("Inside Method : validateSettingsOnDevice()");
		String defaultValue = null;
		for (FactoryResetSettings factoryResetSetting : factoryResetSettings) {

			if (!skipList.contains(factoryResetSetting)) {
				defaultValue = defaultValues.get(factoryResetSetting);
				LOGGER.info("Default value for the parameter " + factoryResetSetting + " is " + defaultValue);
				if (CommonMethods.isNotNull(defaultValue)) {
					String response = null;
					if (factoryResetSetting.equals(WifiSsidSnmp.WIFI_SSID_2_4)
							|| factoryResetSetting.equals(WifiSsidSnmp.WIFI_SSID_5)) {
						LOGGER.info("Inside wifi ssid value for snmp");
						response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
								BroadBandSnmpMib.ECM_CURRENT_SSID.getOid(),
								factoryResetSetting.getSystemDefaultVariable());
						LOGGER.info("SNMP Response for current value of parameter " + factoryResetSetting + ": "
								+ response);
					} else {
						response = tapEnv.executeWebPaCommand(device, factoryResetSetting.getWebpaParameter());
					}

					if (CommonMethods.isNotNull(response)
							&& !CommonMethods.patternMatcher(response,
									BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OBJECT)
							&& !CommonMethods.patternMatcher(response,
									BroadBandSnmpConstants.SNMP_ERROR_RESPONSE_NO_OID)
							&& !CommonMethods.patternMatcher(response,
									BroadBandSnmpConstants.SNMP_ERROR_NON_WRITABLE)) {
						if (response.equalsIgnoreCase(defaultValue)) {
							message.append(
									"SUCCESS : Current value and Default value after factory reset for the parameter "
											+ factoryResetSetting + " is same."
											+ BroadBandTestConstants.DELIMITER_NEW_LINE);
						} else {
							message.append(
									"FAILED : Current value and Default value after factory reset for the parameter "
											+ factoryResetSetting + " is not same."
											+ BroadBandTestConstants.DELIMITER_NEW_LINE);
						}
					} else {
						message.append(
								"FAILED: Current Value is not obtained for parameter " + factoryResetSetting.toString()
										+ " Skipping step to validate value after factory reset "
										+ BroadBandTestConstants.DELIMITER_NEW_LINE);
					}
				} else {
					message.append("FAILED: Default Value is not obtained for parameter "
							+ factoryResetSetting.toString() + " Skipping step to validate value after factory reset n"
							+ BroadBandTestConstants.DELIMITER_NEW_LINE);
				}
			} else {
				message.append("FAILED: Failed to set value before factory reset for the parameter "
						+ factoryResetSetting.toString() + " Skipping step to validate value after factory reset "
						+ BroadBandTestConstants.DELIMITER_NEW_LINE);
			}

		}
		LOGGER.info(message.toString());
		LOGGER.debug("Exit from Method : validateSettingsOnDevice()");
		return message.toString();
	}

	/**
	 * Method to populate default Values of SSID for DSL device before factory reset
	 * 
	 * @param device Dut instance
	 * @param tapEnv AutomaticsTapApi instance
	 * @refactor Athira
	 */
	public void populateDefaultValuesForSSID(Dut device, AutomaticsTapApi tapEnv) {
		String response_2GHz = null;
		String response_5GHz = null;

		response_2GHz = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ);
		response_5GHz = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ);

		defaultValuesForSSID = new HashMap<String, String>();
		defaultValuesForSSID.put(BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ, response_2GHz);
		defaultValuesForSSID.put(BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ, response_5GHz);
	}

	/**
	 * Method for returning wifi SSID matching status after factory reset
	 * 
	 * @param ssId     SSID to be checked
	 * @param response response for webPA
	 * @return wifi SSID matching status after factory reset
	 * @refactor Athira
	 */
	private boolean verifySSIDStatus(String ssId, String response) {
		boolean status = false;
		if (null != defaultValuesForSSID) {
			String response_GHz = defaultValuesForSSID.get(ssId);
			if (CommonMethods.isNotNull(response_GHz) && CommonMethods.isNotNull(response)) {
				status = response.equalsIgnoreCase(response_GHz);
			}
		}
		return status;
	}

	/**
	 * Method to verify default values after factory reset
	 * 
	 * @param settop             instance of {@link Dut}
	 * @param testId             Test case ID
	 * @param isFiberDevice
	 * @param isSpecificDevice flag value for Bussinessclass device models
	 * 
	 * @refactor Athira
	 * 
	 */
	public static void verifyDefaultValuesAfterFactoryReset(Dut device, String testId, int stepNumber,
			boolean isFiberDevice, boolean isSpecificDevice) {

		// List to store the default values in 2.4Ghz band
		Map<String, String> valuesOfParametersIn2GhzBand = null;
		// List to store the default values in 5Ghz band
		Map<String, String> valuesOfParametersIn5GhzBand = null;
		// Variable to store device model
		String testStepNumber = null;
		String errorMessage = null;
		boolean status = false;
		Map<String, String> defaultSsidNames = null;
		boolean applicableModel = false;
		boolean isDSLDevice = DeviceModeHandler.isDSLDevice(device);
		/**
		 * Verify Last reboot reason in files BootTime.log/PARODUSlog.txt.0 EXPECTED:
		 * Reboot reason should be factory-reset
		 */

		applicableModel = BroadBandCommonUtils.applicableDeviceVerification(device,
				BroadBandTestConstants.WPA3_PERSONAL_TRANSITION_MODELS);
		// STEP 8

		/**
		 * Verify Last reboot reason using webpa command
		 */
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Failed to get last reboot reason from log files BootTime.log or PARODUSlog.txt.0";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ":DESCRIPTION:Verify Last reboot reason from BootTime.log/PARODUSlog.txt.0 file");
		LOGGER.info("STEP " + stepNumber + ":ACTION: Execute command:grep -i \"reboot\"  /rdklogs/logs/WEBPA.txt.0");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED: Reboot reason should be factory-reset");
		LOGGER.info("**********************************************************************************");
		long startTime = System.currentTimeMillis();
		do {
			try {
				errorMessage = "Failed to get Last reboot reason as"
						+ BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET
						+ "from the BootTime.log or Parodus log file.";
				status = BroadBandCommonUtils.pollForSTBAccessibility(tapEnv, device,
						BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS)
						&& BroadBandCommonUtils.verifyLastRebootReasonFromBootTimeOrParodusLogFile(device, tapEnv);
				if (!status) {
					// Verifying the Reboot Reason from device trace
					status = CommonUtils.validateTraceLog(tapEnv, device,
							BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, true);
				}
			} catch (Exception exception) {
				errorMessage += exception.getMessage();
				LOGGER.error(errorMessage);
			}

		} while (!status && ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS)
				&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
		BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " ACTUAL : Reboot reason is factory-reset in BootTime.log or PARODUSlog.txt.0 log files");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify Last reboot reason using webpa command EXPECTED: Reboot reason should
		 * be factory-reset
		 */
		// STEP 9
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Failed to get the \"Last reboot reason\" obtained using webpa Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason. Expected : 'factory-reset', Actual : ";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ":DESCRIPTION:Verify Last reboot reason using webpa command \"Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason\"");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION:Execute the  WEBPA params-Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason and verify reboot reason ");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED: Reboot reason should be factory-reset");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_LAST_REBOOT_REASON,
					BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET);
		} catch (TestException e) {
			status = false;
			errorMessage += e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : Reboot reason is factory-reset");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * verify the default SSID Name for 2.4 GHz band using WebPA EXPECTED : It must
		 * be of format XFSETUP-[last four hexadecimal digits of ecm mac]
		 * 
		 */
		// STEP 10
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		BroadBandResultObject broadBandResultObject = null;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: verify the default SSID Name for 2.4 GHz band using WebPA");
		LOGGER.info("STEP " + stepNumber + ":ACTION : Execute the WebPA command Device.WiFi.SSID.10001.SSID");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : It must contain large four digits of ECM mac");
		LOGGER.info("**********************************************************************************");
		defaultSsidNames = tapEnv.executeMultipleWebPaGetCommands(device,
				BroadBandWebPaConstants.PARAMETERS_FOR_SSID_NAMES);
		LOGGER.info("Obtained default ssid names as -"
				+ defaultSsidNames.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME));
		errorMessage = "Default SSID name in 2.4Ghz is not as Expected., Expected : " + defaultSsidNames
				+ ", Actual : ";

		defaultSsidNames = tapEnv.executeMultipleWebPaGetCommands(device,
				BroadBandWebPaConstants.PARAMETERS_FOR_SSID_NAMES);
		LOGGER.info("Obtained default ssid names as -" + defaultSsidNames);
		try {
			broadBandResultObject = BroadBandRestoreWifiUtils.verifyDefaultSsidForAllPartners(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			status = broadBandResultObject.isStatus();
			errorMessage = broadBandResultObject.getErrorMessage();
		} catch (TestException e) {
			errorMessage += e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : It  contains large four digits of ECM mac");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * verify the default SSID Name for 5 GHz band using WebPA EXPECTED : It must
		 * contain large four digits of ECM mac
		 */

		// STEP 11
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Default SSID name in 5Ghz is not as Expected., Expected : "
				+ defaultSsidNames.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME)
				+ ", Actual : ";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: verify the default SSID Name for 5 GHz band using WebPA");
		LOGGER.info("STEP " + stepNumber + ":ACTION : Execute the WebPA command Device.WiFi.SSID.10101.SSID");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : It must contain large four digits of ECM mac");
		LOGGER.info("**********************************************************************************");
		try {
			broadBandResultObject = BroadBandRestoreWifiUtils.verifyDefaultSsidForAllPartners(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			status = broadBandResultObject.isStatus();
			errorMessage = broadBandResultObject.getErrorMessage();
		} catch (TestException e) {
			errorMessage += e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : It  contains large four digits of ECM mac");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Re activate the device using WebPA EXPECTED : All the related webpa params
		 * should be set successfully
		 */

		// STEP 12
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Unable to personalise the deivce after factory reset";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Personalise device after factory reset using WebPA.");
		LOGGER.info("STEP " + stepNumber + ":ACTION: Execute webpa commands to Reactivate the device ");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : All the related webpa params should be set successfully");
		LOGGER.info("**********************************************************************************");
		startTime = System.currentTimeMillis();
		do {
			try {
				BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
				status = true;
			} catch (Exception exception) {
				LOGGER.error("Exception occured during execution => " + exception.getMessage());
			}
		} while (!status && ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS)
				&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " ACTUAL : The webPA response for Reactivation should be success for all the parameters");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * verify Wifi network security mode for 2.4 GHz band EXPECTED : The security
		 * mode should be 'WPA2-Personal'
		 */

		// STEP 13
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		boolean axModeApplicableDevices = false;
		errorMessage = "Security mode is not changed to default mode (WPA2-Personal) after factory reset";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION:verify Wifi network security mode for 2.4 GHz band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute webpa commands-Device.WiFi.AccessPoint.10001.Security.ModeEnabled,Device.WiFi.AccessPoint.10001.Security.X_CISCO_COM_EncryptionMethod to get network security mode");
		LOGGER.info("STEP " + stepNumber
				+ ":EXPECTED :The values of parameters Device.WiFi.AccessPoint.10001.Security.ModeEnabled and Device.WiFi.AccessPoint.10001.Security.X_CISCO_COM_EncryptionMethod 'WPA2-Personal' and 'AES' respectively");
		LOGGER.info("**********************************************************************************");
		RdkBWifiParameters paramValue = null;
		try {

			axModeApplicableDevices = BroadBandCommonUtils.applicableDeviceVerification(device,
					BroadBandTestConstants.ENABLE_AX_MODE_DEVICES);
			if (axModeApplicableDevices) {
				axModeApplicableDevices = BroadBandWiFiUtils.enableAxMode(device, tapEnv);
			}
			valuesOfParametersIn2GhzBand = tapEnv.executeMultipleWebPaGetCommands(device,
					BroadBandWebPaConstants.PARAMETERS_FOR_2_4_GHZ_BAND);
			LOGGER.info("Responses for parameters for 2GHz: " + valuesOfParametersIn2GhzBand.toString());

			if (BroadbandPropertyFileHandler.isApplicableDeviceModel(device)) {

				status = BroadBandCommonUtils.verifyFeatureEnableViaRFC(device, tapEnv,
						BroadBandTestConstants.PATTERN_GET_WEBPA_PARAM_RFC_TRANSITION_ENABLE,
						BroadBandTestConstants.CONSTANT_BOOL_CHECK, BroadBandTestConstants.TRUE);
				LOGGER.info("WEBPA_PARAM_WPA3_TRANSITION_ENABLE value is enabled via RFC?  " + status);
				applicableModel = status;
				LOGGER.info("applicableModel boolean after verifyFeatureEnableViaRFC : " + applicableModel);
			}
			paramValue = BroadBandCommonUtils.getWiFiParamForDeviceModels(device,
					applicableModel ? RdkBWifiParameters.SECURITY_MODE_2GHZ_PRIVATE_WIFI_WPA3
							: RdkBWifiParameters.SECURITY_MODE_2GHZ_PRIVATE_WIFI);
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand, paramValue)
					&& BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand,
							RdkBWifiParameters.ENCRYPTION_MODE_2GHZ_PRIVATE_WIFI);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " ACTUAL : The values of parameters Device.WiFi.AccessPoint.10001.Security.ModeEnabled and Device.WiFi.AccessPoint.10001.Security.X_CISCO_COM_EncryptionMethod  is 'WPA2-Personal' and 'AES' respectively");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify Wifi network enabled status for 2.4 GHz Wifi band EXPECTED : The
		 * network enabled status should be 'Up'
		 */
		// STEP14
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Wifi network is not enabled for 2.4 GHz after factory reset and wifi personalisation ";

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify Wifi network enabled status for 2.4 GHz Wifi band ");
		LOGGER.info("STEP " + stepNumber + ":ACTION: Execute the webPA  command -Device.WiFi.SSID.10001.Status");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED :The network enabled status should be 'Up'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand,
					RdkBWifiParameters.SSID_STATUS_2GHZ_PRIVATE_WIFI);
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : The network enabled status should is 'Up'");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify status of auto channel selection in 2.4 GHz Wifi band EXPECTED : auto
		 * channel selection should be enabled in 2.4Ghz
		 */
		// STEP 15
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Auto channel selection is not enabled for 2.4 GHz after factory reset";

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP " + stepNumber + ":DESCRIPTION: Verify status of auto channel selection in 2.4 GHz Wifi band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.Radio.10000.AutoChannelEnable");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : auto channel selection should be enabled in 2.4Ghz");
		LOGGER.info("**********************************************************************************");

		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand,
					RdkBWifiParameters.AUTOCHANNEL_ENABLE_STATUS_2GHZ);
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : auto channel selection is enabled in 2.4Ghz");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify network channel bandwidth for 2.4 GHz Wifi band EXPECTED : network
		 * channel bandwidth for 2.4 GHz Wifi band should be '20MHz'
		 */
		// STEP 16
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Network channel bandwidth for 2.4 GHz Wifi band is not '20MHz'";

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION:  Verify network channel bandwidth for 2.4 GHz Wifi band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.Radio.10000.OperatingChannelBandwidth");
		LOGGER.info(
				"STEP " + stepNumber + ":EXPECTED : network channel bandwidth for 2.4 GHz Wifi band should be '20MHz'");
		LOGGER.info("**********************************************************************************");
		try {
			paramValue = BroadBandCommonUtils.getWiFiParamForDeviceModels(device,
					RdkBWifiParameters.OPERATING_CHANNEL_BANDWIDTH_2GHZ);
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand, paramValue);

		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : network channel bandwidth for 2.4 GHz Wifi band is '20MHz'");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify Wifi Broadcast network Name enabled status for 2.4 GHz Wifi band
		 * EXPECTED : Broadcast network Name enabled status for 2.4 GHz Wifi band must
		 * be true
		 */
		// STEP17
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Broadcast network Name enabled status for 2.4 GHz Wifi band is not true";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ":DESCRIPTION:  Verify Wifi Broadcast network Name enabled status for 2.4 GHz Wifi band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled");
		LOGGER.info("STEP " + stepNumber
				+ ":EXPECTED : Broadcast network Name enabled status for 2.4 GHz Wifi band must be true");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand,
					RdkBWifiParameters.SSID_ADVERTISEMENT_ENABLE_2GHZ_PRIVATE_WIFI);

		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " ACTUAL : Broadcast network Name enabled status for 2.4 GHz Wifi band is true");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify Guard interval for 2.4 GHz Wifi network EXPECTED : Guard interval for
		 * 2.4 GHz Wifi network must be 'Auto'
		 * 
		 */
		// STEP 18
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Default Guard interval for 2.4 GHz Wifi network is not 'Auto'";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify Guard interval for 2.4 GHz Wifi network");
		LOGGER.info(
				"STEP " + stepNumber + ":ACTION: Execute the webPA  command -Device.WiFi.Radio.10000.GuardInterval");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : Guard interval for 2.4 GHz Wifi network must be 'Auto'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand,
					RdkBWifiParameters.GUARD_INTERVAL_2GHZ);
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : Guard interval for 2.4 GHz Wifi network must be 'Auto'");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify WiFi network active status for 2.4 GHz in Connection Status page
		 * EXPECTED : WiFi network active status must be 'true'
		 */

		// STEP 19
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "After personalising device 2.4 GHz wifi network is not active";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ":DESCRIPTION: Verify WiFi network active status for 2.4 GHz in Connection Status page");
		LOGGER.info("STEP " + stepNumber + ":ACTION: Execute the webPA  command -Device.WiFi.SSID.10001.Enable");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : WiFi network active status must be 'true'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand,
					RdkBWifiParameters.SSID_ENABLE_STATUS_2GHZ_PRIVATE_WIFI);
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : WiFi network active status is'true'");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify the WiFi supported protocols for 2.4Ghz band EXPECTED : supported
		 * protocols for 2.4Ghz band must be 'g,n'
		 */
		// STEP20
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Default Wifi operating standard for 2.4 GHz is not g,n after factory reset";
		LOGGER.info("****************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify the WiFi supported protocols for 2.4Ghz band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.Radio.10000.OperatingStandards");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : supported  protocols for 2.4Ghz band must be 'g,n'");
		LOGGER.info("****************************************************************");
		try {
			paramValue = BroadBandCommonUtils.getWiFiParamForDeviceModels(device,
					RdkBWifiParameters.OPERATING_STANDARDS_2GHZ);
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn2GhzBand, paramValue);
		} catch (TestException e) {

			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : supported  protocols for 2.4Ghz band is 'g,n' or 'g,n,ax'");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify Wifi network security mode for 5 GHz band EXPECTED : The values of
		 * parameters Device.WiFi.AccessPoint.10001.Security.ModeEnabled and
		 * Device.WiFi.AccessPoint.10001.Security.X_CISCO_COM_EncryptionMethod
		 * 'WPA2-Personal' and 'AES' respectively
		 */
		// STEP21
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Default security mode related parameter values in 5Ghz are not as expected";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify Wifi network security mode for 5 GHz band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.AccessPoint.10101.Security.ModeEnabled,Device.WiFi.AccessPoint.10101.Security.X_CISCO_COM_EncryptionMethod");
		LOGGER.info("STEP " + stepNumber
				+ ":EXPECTED : The values of parameters Device.WiFi.AccessPoint.10001.Security.ModeEnabled and Device.WiFi.AccessPoint.10001.Security.X_CISCO_COM_EncryptionMethod 'WPA2-Personal' and 'AES' respectively");
		LOGGER.info("**********************************************************************************");
		try {
			valuesOfParametersIn5GhzBand = tapEnv.executeMultipleWebPaGetCommands(device,
					BroadBandWebPaConstants.PARAMETERS_FOR_5GHZ_BAND);
			LOGGER.info("Responses for parameters for 5 GHz:  " + valuesOfParametersIn5GhzBand.toString());

			if (BroadbandPropertyFileHandler.isApplicableDeviceModel(device)) {

				status = BroadBandCommonUtils.verifyFeatureEnableViaRFC(device, tapEnv,
						BroadBandTestConstants.PATTERN_GET_WEBPA_PARAM_RFC_TRANSITION_ENABLE,
						BroadBandTestConstants.CONSTANT_BOOL_CHECK, BroadBandTestConstants.TRUE);
				LOGGER.info("WEBPA_PARAM_WPA3_TRANSITION_ENABLE value is enabled via RFC?  " + status);
				applicableModel = status;
				LOGGER.info("applicableModel boolean after verifyFeatureEnableViaRFC : " + applicableModel);
			}
			paramValue = BroadBandCommonUtils.getWiFiParamForDeviceModels(device,
					applicableModel ? RdkBWifiParameters.SECURITY_MODE_5GHZ_PRIVATE_WIFI_WPA3
							: RdkBWifiParameters.SECURITY_MODE_5GHZ_PRIVATE_WIFI);
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand, paramValue)
					&& BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand,
							RdkBWifiParameters.ENCRYPTION_MODE_5GHZ_PRIVATE_WIFI);

		} catch (Exception e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " ACTUAL : The values of parameters Device.WiFi.AccessPoint.10001.Security.ModeEnabled and Device.WiFi.AccessPoint.10001.Security.X_CISCO_COM_EncryptionMethod is 'WPA2-Personal' and 'AES' respectively");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify Wifi network enabled status for 5 GHz Wifi band EXPECTED : The network
		 * enabled status should be 'Up'
		 */

		// step 22
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = " After personalising device wifi network status(private SSID) is not enabled for 5 GHs";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify Wifi network enabled status for 5 GHz Wifi band ");
		LOGGER.info("STEP " + stepNumber + ":ACTION: Execute the webPA  command -Device.WiFi.SSID.10101.Status");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : The network enabled status should be 'Up'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand,
					(isDSLDevice ? RdkBWifiParameters.AUTOCHANNEL_ENABLE_STATUS_5GHZ_DSL
							: RdkBWifiParameters.AUTOCHANNEL_ENABLE_STATUS_5GHZ));
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : The network enabled status is 'Up'");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify status of auto channel selection in 5 GHz Wifi band EXPECTED : auto
		 * channel selection should be enabled in 5Ghz
		 */

		// STEP23
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = " After personalising device auto channel (private SSID) is not enabled for 5 GHs";
		LOGGER.info("****************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify status of auto channel selection in 5 GHz Wifi band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.Radio.10100.AutoChannelEnable");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : auto channel selection should be enabled in 5Ghz");
		LOGGER.info("****************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand,
					(DeviceModeHandler.isDSLDevice(device) ? RdkBWifiParameters.AUTOCHANNEL_ENABLE_STATUS_5GHZ_DSL
							: RdkBWifiParameters.AUTOCHANNEL_ENABLE_STATUS_5GHZ));
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : auto channel selection should be enabled in 5Ghz");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify network channel bandwidth for 5 GHz Wifi band EXPECTED : network
		 * channel bandwidth for 5 GHz Wifi band should be '80MHz'
		 */
		// STEP24
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Default channel bandwidth for 5 GHz(private) wifi is not '40MHz' for cisco Dpc3939 and not 80MHz for other devices";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify network channel bandwidth for 5 GHz Wifi band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.Radio.10100.OperatingChannelBandwidth");
		LOGGER.info("STEP " + stepNumber
				+ ":EXPECTED : network channel bandwidth for 5 GHz Wifi band should be '40MHz' for cisco Dpc3939 and 80MHz for other devices.");
		LOGGER.info("**********************************************************************************");

		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand,
					(isSpecificDevice ? RdkBWifiParameters.OPERATING_CHANNEL_BANDWIDTH_5GHZ_FOR_SPECIFIC_DEVICES
							: RdkBWifiParameters.OPERATING_CHANNEL_BANDWIDTH_5GHZ));
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : network channel bandwidth for 5 GHz Wifi band is "
					+ (isSpecificDevice ? "40MKz" : "80MHz"));
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi band
		 * EXPECTED : Broadcast network Name enabled status for 5 GHz Wifi band must be
		 * true
		 */
		// STEP 25
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Broadcast network name is not enabled for 5 GHz wifi (private)";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ":DESCRIPTION: Verify Wifi Broadcast network Name enabled status for 5 GHz Wifi band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled");
		LOGGER.info("STEP " + stepNumber
				+ ":EXPECTED : Broadcast network Name enabled status for 5 GHz Wifi band must be true");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand,
					RdkBWifiParameters.SSID_ADVERTISEMENT_ENABLE_5GHZ_PRIVATE_WIFI);
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " ACTUAL : Broadcast network Name enabled status for 5 GHz Wifi band is true");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify Guard interval for 5 GHz Wifi network EXPECTED : Guard interval for 5
		 * GHz Wifi network must be 'Auto'
		 * 
		 */
		// STEP 26
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Default Guard interval for 5 GHz Wifi network is not 'Auto'";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify Guard interval for 5 GHz Wifi network");
		LOGGER.info(
				"STEP " + stepNumber + ":ACTION: Execute the webPA  command -Device.WiFi.Radio.10100.GuardInterval");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : Guard interval for 5 GHz Wifi network must be 'Auto'");
		LOGGER.info("**********************************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand,
					RdkBWifiParameters.GUARD_INTERVAL_5GHZ);
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL : Guard interval for 5 GHz Wifi network must be 'Auto'");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify WiFi network active status for 5 GHz in Connection Status page
		 * EXPECTED : WiFi network active status must be 'true'
		 */

		// STEP 27
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "After personalising device 5 GHz wifi network is not active";
		LOGGER.info("****************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ":DESCRIPTION: Verify WiFi network active status for 5 GHz in Connection Status page");
		LOGGER.info("STEP " + stepNumber + ":ACTION: Execute the webPA  command -Device.WiFi.SSID.10101.Enable");
		LOGGER.info("STEP " + stepNumber + ":EXPECTED : WiFi network active status must be 'true'");
		LOGGER.info("****************************************************************");
		try {
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand,
					RdkBWifiParameters.SSID_ENABLE_STATUS_5GHZ_PRIVATE_WIFI);
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " ACTUAL :  WiFi network active status must be 'true'");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		/**
		 * Verify the WiFi supported protocols for 5Ghz band EXPECTED : supported
		 * protocols for 5Ghz band must be 'a,n,ac'
		 */

		// STEP 28
		++stepNumber;
		testStepNumber = "s" + stepNumber;
		status = false;
		errorMessage = "Default Wifi operating standard for 5 GHz is not a,n,ac after factory reset";
		LOGGER.info("****************************************************************");
		LOGGER.info("STEP " + stepNumber + ":DESCRIPTION: Verify the WiFi supported protocols for 5Ghz band");
		LOGGER.info("STEP " + stepNumber
				+ ":ACTION: Execute the webPA  command -Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP " + stepNumber
				+ ":EXPECTED : supported  protocols for 5Ghz band must be 'a,n'for some models and 'a,n,ac'for other models & 'a,n,ac,ax' for AX supported models");
		LOGGER.info("****************************************************************");
		// ; errorMessage = "Default Wifi operating standard for 2.4 GHz is not a,n,ac
		// after factory reset";
		try {
			paramValue = BroadBandCommonUtils.getWiFiParamForDeviceModels(device,
					RdkBWifiParameters.OPERATING_STANDARDS_5GHZ);
			status = BroadBandCommonUtils.verifyRdkbWifiParameters(valuesOfParametersIn5GhzBand, paramValue);

		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " ACTUAL : supported  protocols for 5Ghz band is 'a,n'for some models and 'a,n,ac'for other models and 'a,n,ac,ax' for ax supported model");
		} else {
			LOGGER.error("STEP " + stepNumber + "ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	}
}
