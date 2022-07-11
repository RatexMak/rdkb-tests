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
package com.automatics.rdkb.tests.snmp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.constants.SnmpConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.error.ErrorType;
import com.automatics.exceptions.TestException;
import com.automatics.providers.objects.DeviceObject;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandSnmpConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.enums.BroadBandWhixEnumConstants.WEBPA_AP_INDEXES;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.snmp.SnmpProtocol;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;

public class BroadBandSnmpTest extends AutomaticsTestBase {

	/**
	 * Verify system description, Modem Configuration Filename,EMTA Address,Cable
	 * Interface MAC Address,Serial Number via SNMP and cross validate with WEBPA
	 * <ol>
	 * <li>STEP 1: Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command
	 * output and check whether it meets the CableLabs specifications</li>
	 * <li>STEP 2: Verify Retrieving the Software version via WEBPA Parameter:
	 * Device.DeviceInfo.SoftwareVersion and Cross verify with the Value Retrieved
	 * from the SNMP value in Step 1.</li>
	 * <li>STEP 3: Verify Retrieving the manufacturer name via WEBPA Parameter:
	 * Device.DeviceInfo.Manufacturer and Cross verify with the Value Retrieved from
	 * the SNMP value in Step 1.</li>
	 * <li>STEP 4: Verify Retrieving the Model name via WEBPA Parameter:
	 * Device.DeviceInfo.ModelName and Cross verify with the Value Retrieved from
	 * the SNMP value in Step 1.</li>
	 * <li>STEP 5: Verify Retrieving the Boot Loader Version via WEBPA Parameter:
	 * Device.DeviceInfo.X_CISCO_COM_BootloaderVersion and Cross verify with the
	 * Value Retrieved from the SNMP value in Step 1.</li>
	 * <li>STEP 6: Verify Retrieving the Hardware Version via WEBPA Parameter:
	 * Device.DeviceInfo.Hardwareversion and Cross verify with the Value Retrieved
	 * from the SNMP value in Step 1.</li>
	 * <li>STEP 7: Verify Retrieving the Serial Number via SNMP using:
	 * 1.3.6.1.2.1.69.1.1.4.0 MIB and cross verify the value with the response
	 * retrieved via WEBPA Parameter:Device.DeviceInfo.SerialNumber.</li>
	 * <li>STEP 8: Verify Retrieving the Cable Interface MAC Address via SNMP
	 * using:1.3.6.1.2.1.2.2.1.6.2 MIB and cross verify the value with the response
	 * retrieved via WEBPA Parameter: Device.X_CISCO_COM_CableModem.MACAddress.</li>
	 * <li>STEP 9: Verify Retrieving the Modem Configuration Filename via SNMP
	 * using: 1.3.6.1.2.1.69.1.4.5.0 MIB and cross verify the value with the
	 * response retrieved via WEBPA Parameter:
	 * Device.X_CISCO_COM_CableModem.BootFileName</li>
	 * <li>STEP 10: Verify Retrieving the EMTA Address via SNMP using:
	 * 1.3.6.1.2.1.2.2.1.6.16 MIB and cross verify the value with the response
	 * retrieved via WEBPA Parameter: Device.X_CISCO_COM_MTA.MACAddress.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Vignesh
	 * @Author Athira
	 *
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SNMP_OPERATIONS)
	@TestDetails(testUID = "TC-RDKB-SNMP-1001")
	public void testToVerifySnmpOnDeviceParameters(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-SNMP-101";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		String snmpSystemDescrOutput = null;
		String softwareVersion = null;
		String manufacturerName = null;
		String manufacturerNameRetrievedViaWebpa = null;
		String modelName = null;
		String bootLoader = null;
		String hardwareVersion = null;
		String serialNumber = null;
		String cableInterfaceMacAddress = null;
		String modemConfigurationFilename = null;
		String emtaAddress = null;

		BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1001");
		LOGGER.info(
				"TEST DESCRIPTION: Verify system description, Modem Configuration Filename,EMTA Address,Cable Interface MAC Address,Serial Number via SNMP and cross validate with WEBPA");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command output and check whether it meets the CableLabs specifications");
		LOGGER.info(
				"2. Verify Retrieving the Software version via WEBPA Parameter: Device.DeviceInfo.SoftwareVersion and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
		LOGGER.info(
				"3. Verify Retrieving the manufacturer name via WEBPA Parameter: Device.DeviceInfo.Manufacturer and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
		LOGGER.info(
				"4. Verify Retrieving the Model name via WEBPA Parameter: Device.DeviceInfo.ModelName  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
		LOGGER.info(
				"5. Verify Retrieving the Boot Loader Version via WEBPA Parameter: Device.DeviceInfo.X_CISCO_COM_BootloaderVersion  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
		LOGGER.info(
				"6. Verify Retrieving the Hardware Version via WEBPA Parameter: Device.DeviceInfo.Hardwareversion  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
		LOGGER.info(
				"7. Verify Retrieving the Serial Number via SNMP using: 1.3.6.1.2.1.69.1.1.4.0 MIB and cross verify the value with the response retrieved via WEBPA Parameter:Device.DeviceInfo.SerialNumber.");
		LOGGER.info(
				"8. Verify Retrieving the Cable Interface MAC Address via SNMP using:1.3.6.1.2.1.2.2.1.6.2 MIB and cross verify the value with the response retrieved via WEBPA Parameter:  Device.X_CISCO_COM_CableModem.MACAddress.");
		LOGGER.info(
				"9. Verify Retrieving the Modem Configuration Filename via SNMP using: 1.3.6.1.2.1.69.1.4.5.0 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_CableModem.BootFileName");
		LOGGER.info(
				"10. Verify Retrieving the EMTA Address  via SNMP using: 1.3.6.1.2.1.2.2.1.6.16 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_MTA.MACAddress.");

		LOGGER.info("#######################################################################################");
		try {

			stepNum = "S1";
			errorMessage = "SysDesc doesn't match the cablelabs specification with Five Standard Fields SW_REV, MODEL, BOOTR, HW_REV and VENDOR as expected";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command output and check whether it meets the CableLabs specifications");
			LOGGER.info(
					"STEP 1: ACTION : Execute SNMP command to retrive the SysDscr by using SNMP oid 1.3.6.1.2.1.1.1");
			LOGGER.info(
					"STEP 1: EXPECTED : Should return SysDescr with only FIVE standard  fields - SW_REV, MODEL, BOOTR, HW_REV and VENDOR. Should satisfy CableLabs specifications");
			LOGGER.info("**********************************************************************************");

			/*
			 * Issue SNMP walk command for SysDescr.
			 */
			snmpSystemDescrOutput = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ESTB_SYS_DESCRIPTION.getOid());
			LOGGER.info("ACTUAL : SNMP Response obtained for Command SysDescr is: " + snmpSystemDescrOutput);
			HashMap<String, String> systemDescriptor = BroadBandSnmpUtils
					.parseSystemDescriptorInformationFromSnmpOutput(snmpSystemDescrOutput);
			status = (systemDescriptor.size() == BroadBandTestConstants.ALLOWED_NUMBER_OF_SYS_DESCRIPTOR_FILED);
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : SysDesc matches the cablelabs specification with Five Standard Fields SW_REV, MODEL, BOOTR, HW_REV and VENDOR as expected");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S2";
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify Retrieving the Software version via WEBPA Parameter: Device.DeviceInfo.SoftwareVersion and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
			LOGGER.info(
					"STEP 2: ACTION : \"Execute WEBPA  command retrive to the Software version via WEBPA Parameter: Device.DeviceInfo.SoftwareVersion");
			LOGGER.info(
					"STEP 2: EXPECTED : Software Version Retrieved from WEBPA should be same as the Software Version Value Retrived from SNMP in Step 1.");
			LOGGER.info("**********************************************************************************");
			softwareVersion = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_SOFTWARE_VERSION);
			LOGGER.info("Software Version Obtained via SNMP Command is :" + softwareVersion);
			errorMessage = "Unable to Obtain Software Version or Software Version Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(softwareVersion)) {
				broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.TR69_PARAM_SOFTWARE_VERSION, softwareVersion,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Software Version Retrieved from WEBPA matches with the Software Version Value Retrived from SNMP in Step 1 as expected");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S3";
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Verify Retrieving the manufacturer name via WEBPA Parameter: Device.DeviceInfo.Manufacturer and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
			LOGGER.info(
					"STEP 3: ACTION : \"Execute WEBPA  command to retrive the manufacturer name via WEBPA Parameter: Device.DeviceInfo.Manufacturer");
			LOGGER.info(
					"STEP 3: EXPECTED : Manufacturer Name Retrieved from WEBPA should be same as the Manufacturer name Value Retrived from SNMP in Step 1.");
			LOGGER.info("**********************************************************************************");
			manufacturerName = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_VENDOR);
			LOGGER.info("Manufacturer Name Obtained via SNMP Command is :" + manufacturerName);
			errorMessage = "Unable to Obtain Manufacturer Name or Manufacturer Name Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(manufacturerName)) {

				try {

					manufacturerNameRetrievedViaWebpa = BroadBandCommonUtils.getAutomaticsPropsValueByResolvingPlatform(
							device, BroadBandTestConstants.MANUFACTURERNAME_VIAWEBPA);

				} catch (Exception e) {
					manufacturerNameRetrievedViaWebpa = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.TR69_PARAM_MANUFACTURER);
					LOGGER.info("manufacturerName Retrieved Via Webpa as no device specific value found");
				}

				LOGGER.info("Manufacturer Name Obtained via WEBPA Command is :" + manufacturerNameRetrievedViaWebpa);
				errorMessage = "Unable to Obtain Manufacturer Name or Manufacturer Name Obtained via WEBPA is null.";
				if (CommonMethods.isNotNull(manufacturerNameRetrievedViaWebpa)) {
					errorMessage = "Manufacturer Name Retrieved from WEBPA doesn't match with the Manufacturer Name Value Retrived from SNMP in Step 1 as expected.";
					status = CommonUtils.patternSearchFromTargetString(manufacturerNameRetrievedViaWebpa,
							manufacturerName);
				}
			}
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Manufacturer Name Retrieved from WEBPA matches with the Manufacturer name Value Retrived from SNMP in Step 1 as expected.");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S4";
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify Retrieving the Model name via WEBPA Parameter: Device.DeviceInfo.ModelName  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
			LOGGER.info(
					"STEP 4: ACTION : \"Execute WEBPA  command to retrive the Model name via WEBPA Parameter: Device.DeviceInfo.ModelName");
			LOGGER.info(
					"STEP 4: EXPECTED : Model Name Retrieved from WEBPA should be same as the Model name Value Retrived from SNMP in Step 1.");
			LOGGER.info("**********************************************************************************");
			modelName = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_MODEL);
			LOGGER.info("Model Name Obtained via SNMP Command is :" + modelName);
			errorMessage = "Unable to Obtain Model Name or Model Name Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(modelName)) {
				broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_MODELNAME, modelName,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Model Name Retrieved from WEBPA matches with the Model Name Value Retrived from SNMP in Step 1 as expected");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S5";
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Verify Retrieving the Boot Loader Version via WEBPA Parameter: Device.DeviceInfo.X_CISCO_COM_BootloaderVersion  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
			LOGGER.info(
					"STEP 5: ACTION : \"Execute WEBPA  command to retrive the Boot Loader Version via WEBPA Parameter: Device.DeviceInfo.X_CISCO_COM_BootloaderVersion");
			LOGGER.info(
					"STEP 5: EXPECTED : Boot Loader Version Retrieved from WEBPA should be same as the Boot Loader Version Value Retrived from SNMP in Step 1.");
			LOGGER.info("**********************************************************************************");
			bootLoader = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_BOOT_LOADER_VERSION);
			LOGGER.info("Boot Loader Obtained via SNMP Command is :" + bootLoader);
			errorMessage = "Unable to Obtain Boot Loader or Boot Loader Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(bootLoader)) {
				broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.TR69_PARAM_DEVICE_INFO_BOOT_LOADER_VERSION, bootLoader,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Boot Loader Retrieved from WEBPA matches with the Boot Loader Value Retrived from SNMP in Step 1 as expected");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S6";
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify Retrieving the Hardware Version via WEBPA Parameter: Device.DeviceInfo.Hardwareversion  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
			LOGGER.info(
					"STEP 6: ACTION : \"Execute WEBPA  command to retrive Hardware Version via WEBPA Parameter: Device.DeviceInfo.Hardwareversion");
			LOGGER.info(
					"STEP 6: EXPECTED : Hardware Version Name Retrieved from WEBPA should be same as the Hardware version name Value Retrived from SNMP in Step 1.");
			LOGGER.info("**********************************************************************************");
			hardwareVersion = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_HARDWARE_VERSION);
			LOGGER.info("Hardware Version Obtained via SNMP Command is :" + hardwareVersion);
			errorMessage = "Unable to Obtain Hardware Version or Hardware Version Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(hardwareVersion)) {
				broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_HARDWARE_VERSION, hardwareVersion,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : Hardware Version Retrieved from WEBPA matches with the Hardware Version Value Retrived from SNMP in Step 1 as expected");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S7";
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 7: DESCRIPTION : Verify Retrieving the Serial Number via SNMP using: 1.3.6.1.2.1.69.1.1.4.0 MIB and cross verify the value with the response retrieved via WEBPA Parameter:Device.DeviceInfo.SerialNumber.");
			LOGGER.info(
					"STEP 7: ACTION : Execute SNMP command to retrive to retrieve the Serial Number (and Cross verify via WEBPA) by using SNMP oid 1.3.6.1.2.1.1.1");
			LOGGER.info(
					"STEP 7: EXPECTED : Serial Number Retrieved from SNMP should be same as the Serial Number Value Retrived via WEBPA.");
			LOGGER.info("**********************************************************************************");

			serialNumber = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SERIAL_NUMBER.getOid(), BroadBandSnmpMib.ECM_SERIAL_NUMBER.getTableIndex());
			LOGGER.info("Serial Number Obtained via SNMP Command is :" + serialNumber);
			errorMessage = "Unable to Obtain Serial Number or Serial Number Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(serialNumber)) {
				broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_SERIAL_NUMBER, serialNumber,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : Serial Number Retrieved from SNMP matches with the Serial Number Value Retrived via WEBPA as expected.");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S8";
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8: DESCRIPTION : Verify Retrieving the Cable Interface MAC Address via SNMP using:1.3.6.1.2.1.2.2.1.6.2 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_CableModem.MACAddress");
			LOGGER.info(
					"STEP 8: ACTION : Execute the SNMP Get Command to retrieve the Serial Number (and Cross verify via WEBPA) by using SNMP oid 1.3.6.1.2.1.2.2.1.6.2");
			LOGGER.info(
					"STEP 8: EXPECTED :  Cable Interface MAC Address Retrieved from SNMP should be same as the Cable Interface MAC Address Value Retrived via WEBPA.");
			LOGGER.info("**********************************************************************************");

			cableInterfaceMacAddress = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CABLE_INTERFACE_MAC_ADDRESS.getOid(),
					BroadBandSnmpMib.ECM_CABLE_INTERFACE_MAC_ADDRESS.getTableIndex());
			LOGGER.info("Cable Interface Mac Address Obtained via SNMP Command is :" + cableInterfaceMacAddress);
			errorMessage = "Unable to Obtain Cable Interface Mac Address or Cable Interface Mac Address Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(cableInterfaceMacAddress)) {
				broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_CM_MAC, cableInterfaceMacAddress,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL : Cable Interface MAC Address Retrieved from SNMP matches with the Cable Interface MAC Address Value Retrived via WEBPA as expected.");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S9";
			errorMessage = null;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Verify Retrieving the Modem Configuration Filename via SNMP using: 1.3.6.1.2.1.69.1.4.5.0 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_CableModem.BootFileName");
			LOGGER.info(
					"STEP 9: ACTION : Execute the SNMP Get Command to retrieve the Serial Number (and Cross verify via WEBPA) by using SNMP oid 1.3.6.1.2.1.69.1.4.5.0");
			LOGGER.info(
					"STEP 9: EXPECTED : Modem Configuration Filename Retrieved from SNMP should be same as the Modem Configuration Filename Value Retrived via WEBPA.");
			LOGGER.info("**********************************************************************************");

			modemConfigurationFilename = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_MODEM_CONFIGURATION_FILENAME.getOid(),
					BroadBandSnmpMib.ECM_MODEM_CONFIGURATION_FILENAME.getTableIndex());
			LOGGER.info("Modem Configuration Filename Obtained via SNMP Command is :" + modemConfigurationFilename);
			errorMessage = "Unable to Obtain Modem Configuration Filename or Modem Configuration Filename Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(modemConfigurationFilename)) {
				broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_BOOT_FILENAME, modemConfigurationFilename,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : Modem Configuration Filename Retrieved from SNMP matches with the Modem Configuration Filename Value Retrived via WEBPA as expected.");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S10";
			errorMessage = "null";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 10: DESCRIPTION : Verify Retrieving the EMTA Address  via SNMP using: 1.3.6.1.2.1.2.2.1.6.16 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_MTA.MACAddress.");
			LOGGER.info(
					"STEP 10: ACTION : Execute the SNMP Get Command to retrieve the Serial Number (and Cross verify via WEBPA) by using SNMP OID 1.3.6.1.2.1.2.2.1.6.16");
			LOGGER.info(
					"STEP 10: EXPECTED : EMTA Address  Retrieved from SNMP should be same as the EMTA Address  Value Retrived via WEBPA.");
			LOGGER.info("**********************************************************************************");

			emtaAddress = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.EMC_EMTA_ADDRESS.getOid(), BroadBandSnmpMib.EMC_EMTA_ADDRESS.getTableIndex());
			LOGGER.info("EMTA Address Obtained via SNMP Command is :" + emtaAddress);
			errorMessage = "Unable to Obtain EMTA Address or EMTA Address Filename Obtained via SNMP is null.";
			if (CommonMethods.isNotNull(emtaAddress)) {
				broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
						tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MTA_MAC, emtaAddress,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
			}
			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : MTA Address Retrieved from SNMP matches with the EMTA Address Value Retrived via WEBPA as expected.");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}

		LOGGER.info(" ENDING TEST CASE: TC-RDKB-SNMP-1001");
	}

	/**
	 * Test to Verify the Manufacturer serial number using
	 * DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) SNMP MIB.
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Selvaraj Mariyappan
	 * @Refactor Athira
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1002")

	public void testSnmpGetOnDocsisCableDeviceSerialNumber(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-002";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;
		String snmpDocsisDeviceSerialNumber = null;
		String manufacturerSerialNumber = null;

		/**
		 * Step 1 : Retrieve the Serial number from the response of
		 * DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber command and verify with the actual
		 * serial number of the device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 1:DESCRIPTION: Retrieve the Serial number from the response of DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber command and verify with the actual serial number of the device using device object. If device object not configured webpa param will be used.");
		LOGGER.info(
				"STEP 1:ACTION: Execute snmp command DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber and cross check the value with the value retrived from CATS/ webpa");
		LOGGER.info("EXPECTED: Serial number should be same for snmp and values retrieved using CATS/webpa");
		LOGGER.info("**********************************************************************************");
		try {

			// Retrieve the serial number from the response
			snmpDocsisDeviceSerialNumber = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCS_CABLE_DEVICE_MIB_DOCS_DEV_SERIAL_NUMBER.getOid());
			LOGGER.info("SNMP command output for DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) : "
					+ snmpDocsisDeviceSerialNumber);
			manufacturerSerialNumber = device.getSerialNumber();
			LOGGER.info("manufacturerSerialNumber obtained from CATS " + manufacturerSerialNumber);
			message = " EXPECTED: Manufacturer Serial Number  : " + manufacturerSerialNumber;

			if (CommonMethods.isNotNull(snmpDocsisDeviceSerialNumber)
					&& CommonMethods.isNotNull(manufacturerSerialNumber)) {
				// Verify with the actual serial number of the device
				status = (snmpDocsisDeviceSerialNumber.trim()).equalsIgnoreCase(manufacturerSerialNumber.trim());
				if (!status) {
					message = "Seems like DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) providing wrong serial number . ACTUAL : Manufacture Serial number :  "
							+ snmpDocsisDeviceSerialNumber + message;
					LOGGER.error(message);
					status = (snmpDocsisDeviceSerialNumber.trim())
							.equalsIgnoreCase(BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
									BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_SERIAL_NUMBER).trim());
					LOGGER.error(
							"Since device object validation failed, Status of serial number validation done via webapa: "
									+ status);
				}
			} else {
				status = false;
				message = "Unable to retrieve manufacturer serial number using DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) SNMP MIB";
				LOGGER.error(message);
			}
			if (status) {
				LOGGER.info("STEP 1 : ACTUAL : Successfully verified serial number");
			} else {
				LOGGER.error("STEP 1 : ACTUAL : " + message);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) details using SNMP MIB"
					+ exception.getMessage();
			LOGGER.error(message);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
		}
	}

	/**
	 * Test to Verify the WAN MAC Address using IF-MIB::ifPhysAddress.1
	 * (.1.3.6.1.2.1.2.2.1.6.1) SNMP MIB.
	 * 
	 * 
	 * <ol>
	 * <li>Step 1 : Retrieve the Mac Address from the response of
	 * IF-MIB::ifPhysAddress.1 command and verify with the actual MAC Address of the
	 * device using device object</li>
	 * </ol>
	 * 
	 * @author Selvaraj Mariyappan
	 * @Refactor Athira
	 * 
	 * @param device {@link Dut}
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1003")
	public void testSnmpGetOnWanMacAddress(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-003";
		String stepNumber = "s1";
		boolean status = false;
		String errorMessage = null;
		String snmpWanMacAddress = null;
		String wanMacAddress = null;
		String ifTableIndex = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1003");
		LOGGER.info(
				"TEST DESCRIPTION:Verify the WAN MAC Address using IF-MIB::ifPhysAddress.1 (.1.3.6.1.2.1.2.2.1.6) ");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1:Verify the Mac Address from the response of IF-MIB::ifPhysAddress.1(for Fibre devices getting the erouter0 mib index) command and verify with the actual MAC Address of the device using device object");
		LOGGER.info("#######################################################################################");

		try {
			errorMessage = "Seems like IF-MIB::ifPhysAddress.1 (.1.3.6.1.2.1.2.2.1.6) providing wrong WAN MAC Address.";
			/**
			 * Step 1 : Retrieve the Mac Address from the response of
			 * IF-MIB::ifPhysAddress.1 command and verify with the actual MAC Address of the
			 * device using device object
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1:DESCRIPTION: Retrieve the Mac Address from the response of IF-MIB::ifPhysAddress.1(for fibre devices getting the erouter0 mib index) command and verify with the actual MAC Address of the device using device object");
			LOGGER.info(
					"STEP 1:ACTION: Execute SNMP command to retrive the WAN MAC Address by using SNMP and compare with Wan Mac address retrived using device object");
			LOGGER.info("STEP 1:EXPECTED: Should return the Device WAN MAC Address");
			LOGGER.info("**********************************************************************************");
			/*
			 * Choosing mib index based on erouter0 interface for Fibre devices.
			 */
			ifTableIndex = BroadBandCommonUtils.getIndexForWanMac(tapEnv, device);
			LOGGER.info("ifTableIndex is " + ifTableIndex);
			if (CommonMethods.isNotNull(ifTableIndex)) {

				if (DeviceModeHandler.isFibreDevice(device)) {
					DeviceObject deviceResponse = new DeviceObject();
					snmpWanMacAddress = CommonMethods.snmpGetOnEstb(tapEnv, device, deviceResponse.getEstbIpAdress(),
							BroadBandSnmpMib.ECM_IF_MIB_IF_PHYSICAL_ADDRESS.getOid() + "." + ifTableIndex);
				} else {

					snmpWanMacAddress = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
							BroadBandSnmpMib.ECM_IF_MIB_IF_PHYSICAL_ADDRESS.getOid(), ifTableIndex);
				}
				LOGGER.info("SNMP command output for the WAN MAC Address is" + snmpWanMacAddress);
				wanMacAddress = BroadBandSnmpUtils.formatMacAddressWithoutLeadingZeros(device.getHostMacAddress());
				if (CommonMethods.isNotNull(snmpWanMacAddress) && CommonMethods.isNotNull(wanMacAddress)) {
					// Verify the retrieved MAC with the actual MAC address of the
					// device
					status = CommonUtils.patternSearchFromTargetString(snmpWanMacAddress, wanMacAddress);
					if (status) {
						LOGGER.info(
								"STEP 1: ACTUAL : retreived Wan MAC address using SNMP and WAN MAC Address using device object are same ");
					} else {
						LOGGER.error("STEP 1: ACTUAL :" + errorMessage + " ACTUAL OUTPUT :  WAN MAC Address :  "
								+ snmpWanMacAddress + " " + " EXPECTED OUTPUT: WAN MAC Address  : " + wanMacAddress);
					}
				} else {
					status = false;
					LOGGER.error("Unable to retrieve IF-MIB::ifPhysAddress." + ifTableIndex
							+ "(.1.3.6.1.2.1.2.2.1.6) details using SNMP MIB");
				}
			} else {
				LOGGER.error(
						"Index to get WAN MAC address is null. Hence Will not able to get WAN MAC Address using SNMP.");
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);
		} catch (Exception exception) {
			status = false;
			LOGGER.error("Unable to retrieve IF-MIB::ifPhysAddress." + ifTableIndex
					+ "(.1.3.6.1.2.1.2.2.1.6) details using SNMP MIB" + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					true);
		}
	}

	/**
	 * Test to Verify the Cable Modem MAC Address using IF-MIB::ifPhysAddress.2
	 * (.1.3.6.1.2.1.2.2.1.6.2) SNMP MIB.
	 * 
	 * @param device The Device to be used.
	 * @author Selvaraj Mariyappan
	 * @Refactor Athira
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1004")

	public void testSnmpGetOnCableModemMacAddress(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-004";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;
		String snmpCableModemMacAddress = null;
		String cableModemMacAddress = null;

		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the Cable Modem MAC Address using IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2)");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1:Retrieve the CM Mac Address from the response of IF-MIB::ifPhysAddress.2 command and verify with the actual MAC Address of the device using device object");
		LOGGER.info("#######################################################################################");

		/**
		 * Step 1 : Retrieve the CM Mac Address from the response of
		 * IF-MIB::ifPhysAddress.2 command and verify with the actual MAC Address of the
		 * device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 1: DESCRIPTION: Retrieve the CM Mac Address from the response of IF-MIB::ifPhysAddress.2 command and verify with the actual MAC Address of the device using device object");
		LOGGER.info(
				"STEP 1:ACTION: Execute SNMP command to retrive the CM MAC Address by using SNMP oid .1.3.6.1.2.1.2.2.1.6.2");

		LOGGER.info("EXPECTED: Should return the Device Cable modem address");
		LOGGER.info("**********************************************************************************");
		try {

			// Retrieve the Cable Modem Mac Address from the SNMP command
			snmpCableModemMacAddress = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_IF_MIB_IF_PHYSICAL_ADDRESS.getOid(), "2");
			LOGGER.info("SNMP command output for IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) : "
					+ snmpCableModemMacAddress);
			/*
			 * Cable-modem MAC address is retrieved from BHC.
			 */
			cableModemMacAddress = BroadBandSnmpUtils
					.formatMacAddressWithoutLeadingZeros(((Device) device).getEcmMac());

			message = " EXPECTED: Cable Modem MAC Address  : " + cableModemMacAddress;

			if (CommonMethods.isNotNull(snmpCableModemMacAddress) && CommonMethods.isNotNull(cableModemMacAddress)) {
				// Verify the retrieved MAC with the actual MAC from CHIMPS
				status = (snmpCableModemMacAddress.trim()).equalsIgnoreCase(cableModemMacAddress.trim());

				if (!status) {
					message = "Seems like IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) providing wrong Cable Modem Address . ACTUAL :  Cable Modem Address :  "
							+ snmpCableModemMacAddress + message;
					LOGGER.error(message);
				}
			} else {
				status = false;
				message = "Unable to retrieve Cable Modem Address using IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) SNMP MIB";
				LOGGER.error(message);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) details using SNMP MIB"
					+ exception.getMessage();
			LOGGER.error(message);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
		}
	}

	/**
	 * Test to Verify the Current software version using
	 * DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) SNMP
	 * MIB.
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Selvaraj Mariyappan
	 * @Refactor Athira
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1005")
	public void testSnmpGetOnDocsisCableDeviceCurrentSoftwareVersion(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-005";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;
		String snmpDocsisDeviceCurrentSoftwareVersion = null;
		String currentDocsisSwVers = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1005");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the Current software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0)");
		LOGGER.info(
				"1. Retrieve the Software version from the response of DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers command and verify with the actual SW version of the device using device object");
		LOGGER.info("#######################################################################################");

		/**
		 * Step 1 : Retrieve the Software version from the response of
		 * DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers command and verify with the
		 * actual SW version of the device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 1: Retrieve the Software version from the response of DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers command and verify with the actual SW version of the device using device object");
		LOGGER.info("EXPECTED: Should return the current software version");
		LOGGER.info("**********************************************************************************");

		try {
			// Retrieve the software version from the SNMP command -
			// DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers
			snmpDocsisDeviceCurrentSoftwareVersion = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCS_DEV_CURRENT_SOFTWARE_VERSION.getOid());
			LOGGER.info(
					"SNMP command output for DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) : "
							+ snmpDocsisDeviceCurrentSoftwareVersion);

			currentDocsisSwVers = BroadBandSnmpUtils.getExpectedDocsisCableDeviceCurrentSoftwareVersion(tapEnv, device);

			message = "\t EXPECTED: Currently running software version  : " + currentDocsisSwVers;

			if (CommonMethods.isNotNull(snmpDocsisDeviceCurrentSoftwareVersion)
					&& CommonMethods.isNotNull(currentDocsisSwVers)) {
				// Verify the retrieved version with the actual firmware version
				// of the device
				status = (snmpDocsisDeviceCurrentSoftwareVersion.trim()).equalsIgnoreCase(currentDocsisSwVers.trim());
				if (!status) {
					message = "Seems like DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) providing wrong currently running software version. ACTUAL : DOCSIS software version :  "
							+ snmpDocsisDeviceCurrentSoftwareVersion + message;
					LOGGER.error(message);
				}
			} else {
				status = false;
				message = "Unable to retrieve Currently running software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) SNMP MIB";
				LOGGER.error(message);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) details using SNMP MIB"
					+ exception.getMessage();
			LOGGER.error(message);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
		}
	}

	/**
	 * Test to verify deviceUpTime using SysUpTime( 1.3.6.1.2.1.1.3.0 ) SNMP MIB and
	 * cross check it with output of 'cat /proc/uptime' command
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Athira
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1006")
	public void testDeviceUpTime(Dut device) {

		String testStepNumber = "s1";
		String testId = "TC-RDKB-SNMP-006";
		String errorMessage = null;
		boolean status = false;

		LOGGER.info("*****************************************************************************************");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1006");
		LOGGER.info(
				" TEST DESCRIPTION: Verify the Current software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0)");
		LOGGER.info(
				"STEP 1: Verify deviceUpTime using SysUpTime( 1.3.6.1.2.1.1.3.0 )SNMP MIB and cross check it with output of cat /proc/uptime command");
		LOGGER.info("*****************************************************************************************");

		LOGGER.info("*****************************************************************************************");
		LOGGER.info(
				"STEP 1: Test to Verify deviceUpTime using SysUpTime( 1.3.6.1.2.1.1.3.0 )SNMP MIB and cross check it with output of cat /proc/uptime command");
		LOGGER.info("EXPECTED: SysUpTime difference between SNMP and cat /proc/uptime should be less than two minutes");
		LOGGER.info("*****************************************************************************************");

		try {
			errorMessage = "Failed to verify system uptime ";
			status = BroadBandSnmpUtils.verifySysUpTime(tapEnv, device);

			if (!status) {
				LOGGER.error(errorMessage);
			}
			LOGGER.info("ACTUAL: Verification of system uptime : " + status);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			status = false;
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1006");

	}

	/**
	 * Test to verify Support Legacy DOCSIS SNR OID
	 * 
	 * <ol>
	 * <li>STEP 1: Execute SNMPWALK command on OID docsIfSigQSignalNoise (
	 * 1.3.6.1.2.1.10.127.1.1.4.1.5).</li>
	 * <li>STEP 2: Execute SNMP v3 SET command on OID
	 * docsIfSigQSignalNoise(1.3.6.1.2.1.10.127.1.1.4.1.5) which is a READ-ONLY
	 * MIB</li>
	 * </ol>
	 * 
	 * @param device The Device to be used.
	 * @author Deepa Bada
	 * @Refactor Athira
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-SIGTONOISE-1021")
	public void testSnmpWalk(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-SIGTONOISE-121";
		String stepNumber = "s1";
		boolean status = false;
		String errorMessage = "Failed to verify DOCSIF SigQSignal Noise";

		try {

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-SIGTONOISE-1021");
			LOGGER.info(
					"TEST DESCRIPTION:Verify the values of DOCS-IF-MIB::docsIfSigQSignalNoise in the given MIB range");
			LOGGER.info("TEST STEPS : ");

			LOGGER.info("1: Execute SNMPWALK command on OID docsIfSigQSignalNoise (1.3.6.1.2.1.10.127.1.1.4.1.5)");
			LOGGER.info(
					"2: Execute SNMP v3 SET command on OID docsIfSigQSignalNoise(1.3.6.1.2.1.10.127.1.1.4.1.5) which is a READ-ONLY");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1 : Execute SNMPWALK command on OID
			 * docsIfSigQSignalNoise(1.3.6.1.2.1.10.127.1.1.4.1.5)
			 * 
			 */

			LOGGER.info("######################################################");
			LOGGER.info(
					"STEP 1 : DESCRIPTION: Verify the values of DOCS-IF-MIB::docsIfSigQSignalNoise in the given MIB range(MIB>=200 and MIB <=500)");
			LOGGER.info("STEP 1 : ACTION: Execute snmpwalk -v2c -c <> <ecm IP> 1.3.6.1.2.1.10.127.1.1.4.1.5");
			LOGGER.info("STEP 1 : EXPECTED: Should return the list of child OID's");
			LOGGER.info("######################################################");
			status = BroadBandSnmpUtils.verifyDocsIfSigQSignalNoiseWithRange(tapEnv, device);

			LOGGER.info(
					"STEP 1: ACTUAL: Verification of OID(DOCSIF SigQSignal Noise) value in range of greater than equal to 200 & less than equal to 500 : "
							+ status);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status,
					BroadBandCommonUtils.concatStringUsingStringBuffer(ErrorType.SNMP.toString(), errorMessage), false);

			/**
			 * Step 2 : Execute SNMP v3 SET command on OID (1.3.6.1.2.1.10.127.1.1.4.1.5)
			 * which is a READ-ONLY MIB
			 * 
			 */

			LOGGER.info("######################################################");
			LOGGER.info("STEP 2 : DESCRIPTION: Verify the READ-ONLY attribute of  DOCS-IF-MIB::docsIfSigQSignalNoise ");
			LOGGER.info(
					"STEP 2 : ACTION: Execute SNMP v3 SET command on OID docsIfSigQSignalNoise(1.3.6.1.2.1.10.127.1.1.4.1.5) which is a READ-ONLY MIB");
			LOGGER.info("STEP 2 : EXPECTED   : Set should not be successful as it’s a read only MIB");
			LOGGER.info("######################################################");

			stepNumber = "s2";
			status = false;

			errorMessage = "Able to do SNMP  SET operation.SNMP Set operation on OID (1.3.6.1.2.1.10.127.1.1.4.1.5)  is expected to fail.";

			status = BroadBandSnmpUtils.verifyReadOnlySignalNoiseStatus(device, tapEnv);

			LOGGER.info("STEP 2: ACTUAL: "
					+ (status ? "The READ-ONLY MIB can not be writable through SNMP" : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status,
					BroadBandCommonUtils.concatStringUsingStringBuffer(ErrorType.SNMP.toString(), errorMessage), false);
		} catch (Exception testException) {
			errorMessage = "Exception occurred validating OID docsIfSigQSignalNoise (1.3.6.1.2.1.10.127.1.1.4.1.5)"
					+ testException.getMessage();
			LOGGER.error(errorMessage);

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status,
					BroadBandCommonUtils.concatStringUsingStringBuffer(ErrorType.SNMP.toString(), errorMessage), false);
		}
	}

	/**
	 * Verify the Software file name, Server transport protocol, Server address type
	 * and Admin status can be retieved from SNMP
	 * <ol>
	 * <li>Verify the Software File name can be retrieved using SNMP</li>
	 * <li>Verify the Server Transport Protocol can be retrieved using SNMP</li>
	 * <li>Verify the Server Address Type can be retrieved using SNMP</li>
	 * <li>Verify the Admin status can be retrieved using SNMP</li>
	 * </ol>
	 * 
	 * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SNMP_OPERATIONS)
	@TestDetails(testUID = "TC-RDKB-SNMP-5001")
	public void verifySnmpGetOnAdminMibs(Dut device) {

		// Variable Declaration begins
		String testCaseId = "";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		String response = null;
		// Variable Declaration Ends

		testCaseId = "TC-RDKB-SNMP-501";

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-5001");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the Software file name, Server transport protocol, Server address type and Admin status can be retieved from SNMP");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify the Software File name can be retrieved using SNMP");
		LOGGER.info("2. Verify the Server Transport Protocol can be retrieved using SNMP");
		LOGGER.info("3. Verify the Server Address Type can be retrieved using SNMP");
		LOGGER.info("4. Verify the Admin status can be retrieved using SNMP");
		LOGGER.info("#######################################################################################");

		try {
			stepNum = "S1";
			errorMessage = "Unable to set/get Software file name from SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify the Software File name can be retrieved using SNMP");
			LOGGER.info(
					"STEP 1: ACTION : Execute the SNMP command to retrive the Software File name using OID .1.3.6.1.2.1.69.1.3.2.0");
			LOGGER.info("STEP 1: EXPECTED : Software file name should be retrieved from SNMP");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("Setting Software File Name inorder to retrieve the same using SNMP");
			String fileName = BroadBandCommonUtils.concatStringUsingStringBuffer(device.getFirmwareVersion(),
					BroadBandTestConstants.BINARY_BUILD_IMAGE_EXTENSION);

			String snmpSetResponse = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid() + ".0", SnmpDataType.STRING, fileName);

			errorMessage = "snmpset on docsDevSwFilename(1.3.6.1.2.1.69.1.3.2.0) with current1 image version as file name failed";
			if (CommonMethods.isNotNull(snmpSetResponse) && snmpSetResponse.equalsIgnoreCase(fileName)) {

				response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
						BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid() + ".0");

				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(fileName);
				errorMessage = "Software Version retrieved from SNMP is null/invalid. Value retrieved from SNMP : "
						+ response;
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Software file name has been successfully retrieved from SNMP");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S2";
			errorMessage = "Unable to retrieve Server Transport Protocol from SNMP";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify the Server Transport Protocol can be retrieved using SNMP");
			LOGGER.info(
					"STEP 2: ACTION : Execute the SNMP command to retrive the Server Transport Protocol using OID 1.3.6.1.2.1.69.1.3.8.0");
			LOGGER.info(
					"STEP 2: EXPECTED : Server Transport protocol should be retrieved from SNMP, the value should be either 1 (TFTP) or 2(HTTP)");
			LOGGER.info("**********************************************************************************");

			response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL.getOid() + ".0");

			status = CommonMethods.isNotNull(response)
					&& (response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE)
							|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO));
			errorMessage = "Server Transport protocol retrieved from SNMP is null/invalid. Value retrieved from SNMP : "
					+ response;
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Server Transport protocol has been successfully retrieved from SNMP & the value is between 1(TFTP) and 2(HTTP)");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S3";
			errorMessage = "Unable to retrieve Server Address Type from SNMP";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify the Server Address Type can be retrieved using SNMP");
			LOGGER.info(
					"STEP 3: ACTION : Execute the SNMP command to retrieve Server Address Type using OID 1.3.6.1.2.1.69.1.3.6.0");
			LOGGER.info(
					"STEP 3: EXPECTED : Server Address Type should be retrieved from SNMP, the value should be either 0(IPv4) or 1 (IPv6) or 2(Hexa Decimal)");
			LOGGER.info("**********************************************************************************");

			response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL.getOid() + ".0");

			status = CommonMethods.isNotNull(response) && (response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO)
					|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE)
					|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO));
			errorMessage = "Server Address Type retrieved from SNMP is null/invalid. Value retrieved from SNMP : "
					+ response;
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Server Address Type has been successfully retrieved from SNMP & the value is either 0(IPv4) or 1 (IPv6) or 2(Hexa Decimal)");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S4";
			errorMessage = "Unable to retrieve Admin status from SNMP";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify the Admin status can be retrieved using SNMP");
			LOGGER.info(
					"STEP 4: ACTION : Execute the SNMP command to retrieve Admin status using OID 1.3.6.1.2.1.69.1.3.3.0");
			LOGGER.info(
					"STEP 4: EXPECTED : Admin status should be retrieved from SNMP, the value should be either 1 or 2 or 3");
			LOGGER.info("**********************************************************************************");

			response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS.getOid() + ".0");

			status = CommonMethods.isNotNull(response)
					&& (response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE)
							|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO)
							|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE));
			errorMessage = "Admin status retrieved from SNMP is null/invalid. Value retrieved from SNMP : " + response;
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Admin status has been successfully retrieved from SNMP & the value is either 1 or 2 or 3");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-5001");
	}

	/**
	 * Test to verify DOCSIS Signal Quality Extended RxMER Table MIB values and its
	 * READ-ONLY attribute
	 * 
	 * <ol>
	 * <li>STEP 1: Verify the values of DOCSIS Signal Quality Extended RxMER Table
	 * MIB are greater than 100</li>
	 * <li>STEP 2: Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMER
	 * MIB</li>
	 * </ol>
	 * 
	 * @param device The device to be used.
	 * @Refactor Alan_Bivera
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1008", testDecription = "Verify DOCSIS Signal Quality Extended RxMER Table MIB values and its READ-ONLY attribute")
	public void testSnmpWalkOnDocsisSignalQualityExtendedRxMerTable(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-108";
		String stepNumber = "s1";
		boolean status = false; // stores the test status
		String errorMessage = null; // stores the error message

		try {
			String snmpWalkOutput = null; // stores SNMP walk output

			/**
			 * Step 1 : Verify the values of DOCSIS Signal Quality Extended RxMER Table MIB
			 * are greater than 100
			 * 
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: Verify the values of DOCSIS Signal Quality Extended RxMER Table MIB are greater than 100");
			LOGGER.info(
					"EXPECTED: On executing this command, SNMP should return a list of values under docsIf3SignalQualityExtRxMER MIB table and all the returned values should be greater than 100");
			LOGGER.info("**********************************************************************************");
			snmpWalkOutput = BroadBandSnmpUtils.snmpWalkOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCSIS_SIGNAL_QUALITY_EXTENDED_RX_MER_TABLE.getOid());
			status = CommonMethods.isNotNull(snmpWalkOutput) && BroadBandSnmpUtils
					.validateSnmpOutputWithExpectedValue(snmpWalkOutput, BroadBandTestConstants.CONSTANT_100);
			errorMessage = "Invalid/Null value retrieved for docsIf3SignalQualityExtRxMER table";
			LOGGER.info("S1 ACTUAL: "
					+ (status ? "All the values under docsIf3SignalQualityExtRxMER table are greater than 100"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 2 : Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMER MIB
			 * 
			 */
			stepNumber = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMER MIB");
			LOGGER.info("EXPECTED: Set should not be successful as its a read only MIB");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(snmpWalkOutput)
					&& BroadBandSnmpUtils.validateReadOnlyAttributeOFDocsisSignalQualityTables(device, tapEnv,
							snmpWalkOutput, BroadBandSnmpMib.ECM_DOCSIS_SIGNAL_QUALITY_EXTENDED_RX_MER_TABLE.getOid(),
							SnmpDataType.INTEGER, BroadBandTestConstants.STRING_VALUE_THREE_HUNDRED_AND_EIGHTY);
			errorMessage = "docsIf3SignalQualityExtRxMER MIB can be writable";
			LOGGER.info(
					"S2 ACTUAL: " + (status ? "All the child OID's under docsIf3SignalQualityExtRxMER MIB are READ ONLY"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

		} catch (Exception testException) {
			errorMessage = "Exception occured while trying to verify DOCSIS Signal Quality Extended RxMER Table MIB values and its READ-ONLY attribute"
					+ testException.getMessage();
			LOGGER.error(errorMessage);

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		}

	}

	/**
	 * Test to verify DOCSIS Signal Quality Extended RxMER Samples Table MIB values
	 * and its READ-ONLY attribute
	 * 
	 * <ol>
	 * <li>STEP 1: Verify the values of DOCSIS Signal Quality Extended RxMER Samples
	 * Table MIB are greater than 0</li>
	 * <li>STEP 2: Verify the READ-ONLY attribute of
	 * docsIf3SignalQualityExtRxMERSamples MIB</li>
	 * </ol>
	 * 
	 * @param device The device to be used.
	 * @Refactor Alan_Bivera
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1009", testDecription = "Verify DOCSIS Signal Quality Extended RxMER Samples Table MIB values and its READ-ONLY attribute")
	public void testSnmpWalkOnDocsisSignalQualityExtendedRxMerSamplesTable(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-109";
		String stepNumber = "s1";
		boolean status = false; // stores the test status
		String errorMessage = null; // stores the error message

		try {
			String snmpWalkOutput = null; // stores SNMP walk output

			/**
			 * Step 1 : Verify the values of DOCSIS Signal Quality Extended RxMER Samples
			 * Table MIB are greater than 100
			 * 
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: Verify the values of DOCSIS Signal Quality Extended RxMER Samples Table MIB are greater than 0");
			LOGGER.info(
					"EXPECTED: On executing this command, SNMP should return a set of values under docsIf3SignalQualityExtRxMerSamples MIB table and all the returned values should be greater than 0");
			LOGGER.info("**********************************************************************************");
			snmpWalkOutput = BroadBandSnmpUtils.snmpWalkOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCSIS_SIGNAL_QUALITY_EXTENDED_RX_MER_SAMPLES_TABLE.getOid());
			status = CommonMethods.isNotNull(snmpWalkOutput) && BroadBandSnmpUtils
					.validateSnmpOutputWithExpectedValue(snmpWalkOutput, BroadBandTestConstants.CONSTANT_0);
			errorMessage = "Invalid/Null value retrieved for docsIf3SignalQualityExtRxMerSamples table";
			LOGGER.info("S1 ACTUAL: "
					+ (status ? "All the values under docsIf3SignalQualityExtRxMerSamples table are greater than 0"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 2 : Verify the READ-ONLY attribute of
			 * docsIf3SignalQualityExtRxMERSamples MIB
			 * 
			 */
			stepNumber = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMerSamples MIB");
			LOGGER.info("EXPECTED: Set should not be successful as its a read only MIB");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(snmpWalkOutput) && BroadBandSnmpUtils
					.validateReadOnlyAttributeOFDocsisSignalQualityTables(device, tapEnv, snmpWalkOutput,
							BroadBandSnmpMib.ECM_DOCSIS_SIGNAL_QUALITY_EXTENDED_RX_MER_SAMPLES_TABLE.getOid(),
							SnmpDataType.UNSIGNED_INTEGER,
							BroadBandTestConstants.STRING_VALUE_FIVE_LAKH_TWENTY_FOUR_THOUSAND);
			errorMessage = "docsIf3SignalQualityExtRxMERSamples MIB can be writable";
			LOGGER.info("S2 ACTUAL: "
					+ (status ? "All the child OID's under docsIf3SignalQualityExtRxMerSamples MIB are READ ONLY"
							: errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

		} catch (Exception testException) {
			errorMessage = "Exception occured while trying to verify DOCSIS Signal Quality Extended RxMER Samples Table MIB values and its READ-ONLY attribute"
					+ testException.getMessage();
			LOGGER.error(errorMessage);

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		}

	}

	/**
	 * Test to validate that the static IPv4 address can be retrieved using
	 * rdkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0)
	 * 
	 * Step 1 : Get the Static WAN IP of the device using SNMP get
	 * 
	 * Step 2: Validate the IP returned in the above response is in line with the
	 * device logs
	 * 
	 * Step 3: Validate the IP returned in the above response is in line with the
	 * TR-181 parameters
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Sathurya Ravi
	 * @refactor anandam
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1024")
	public void testValidateStaticIPv4Address(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-124";
		String stepNumber = "s1";
		boolean status = false;
		String errorMessage = "";
		String response = "";
		String staticIpAddress = "";

		try {

			status = false;
			stepNumber = "s1";
			/**
			 * Step 1 : Get the Static WAN IP of the device using SNMP get
			 */

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Get the Static WAN IP of the device using SNMP get ");
			LOGGER.info(
					"STEP 1: ACTION: Excecute SNMP get on the MIB  rdkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0)");
			LOGGER.info("STEP 1: EXPECTED: The SNMP get response should return the static WAN IP of the device");
			LOGGER.info("**********************************************************************************");

			// Execute SNMP get operation for the MIB dkbRgDeviceConfigStaticIp
			// (.1.3.6.1.4.1.17270.50.2.1.4.6.0)
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getOid(),
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV4.getTableIndex());
			LOGGER.info("SNMP command output for dkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0) : "
					+ response);

			if (CommonMethods.isNotNull(response)) {
				// Validate whether the IP address returned is valid.
				status = CommonMethods.isIpv4Address(response);
				errorMessage = "The IP address returned is not in a proper format. Actual: " + response;
				staticIpAddress = response;
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL: The IP address returned is :" + staticIpAddress);
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}

			} else {
				errorMessage = "The device has returned inappropriate value for the dkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0). Actual: "
						+ response;
				LOGGER.error(errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			status = false;
			stepNumber = "s2";

			/**
			 * Step 2 : Validate the IP returned in the above response is in line with the
			 * device logs
			 */

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION: Validate the IP returned in the above response is in line with the device logs ");
			LOGGER.info(
					"STEP 2: ACTION: Parse through the output of the command cat /etc/ripd.conf by SSHing to the device and get the static WAN IP.");
			LOGGER.info(
					"STEP 2: EXPECTED: The parsed Static IP value and the value from the SNMP response should be the same");
			LOGGER.info("**********************************************************************************");

			// Execute the command cat /etc/ripd.conf by SSHing to the device and get the
			// static WAN IP address
			response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_GET_STATIC_WAN_IP)
					.trim();
			if (CommonMethods.isIpv4Address(response)) {
				// Validate whether the IP address from the logs and from SNMP
				// are the same
				status = response.equals(staticIpAddress);

				errorMessage = "The static WAN IPv4 address from the device logs and SNMP are different. Actual: "
						+ response;
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL: The static WAN IPv4 address from the device logs and SNMP are Same");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}

			} else {
				errorMessage = "The static WAN IPv4 address from the device logs is invalid. Actual: " + response;
				LOGGER.error(errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			status = false;
			stepNumber = "s3";

			/**
			 * Step 3 : Validate the IP returned in the above response is in line with the
			 * TR-181 parameters
			 */

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Validate the IP returned in the above response is in line with the TR-181 parameters ");
			LOGGER.info(
					"STEP 3: ACTION: Parse through the output of the command dmcli eRT getv Device.X_CISCO_COM_TrueStaticIP.IPAddress and get the value of the static WAN IP.");
			LOGGER.info(
					"STEP 3: EXPECTED: The parsed Static IP value and the value from the SNMP response should be the same");
			LOGGER.info("**********************************************************************************");

			// Execute the command dmcli eRT getv
			// Device.X_CISCO_COM_TrueStaticIP.IPAddress by SSHing to the device
			// and
			// get the static WAN IP address

			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_STATIC_WAN_IP);
			if (CommonMethods.isIpv4Address(response)) {

				// Validate whether the IP address from the dmcli command and
				// from SNMP are the same
				status = response.equals(staticIpAddress);
				errorMessage = "The static WAN IPv4 address from the dmcli command and SNMP are different. Actual: "
						+ response;
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL: The static WAN IPv4 address from from the dmcli command is valid ");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
			} else {
				status = false;
				errorMessage = "The static WAN IPv4 address from the dmcli command is invalid. Actual: " + response;
				LOGGER.error(errorMessage);
			}

		} catch (Exception exception) {
			status = false;
			errorMessage = "Unable to validate that static IPv4 address can be retrieved using  rdkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.6.0)"
					+ exception.getMessage();
			LOGGER.error(errorMessage);
		} finally {
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		}
	}

	/**
	 * Verify Ethernet Related SNMP
	 * <ol>
	 * <li>Verify the ethernet port link speed using SNMP MIB.</li>
	 * <li>Verify the Date using SNMP MIB .</li>
	 * <li>Verify device's maximum CEP using SNMP MIB.</li>
	 * <li>Verify DHCP server type of device using SNMP MIB.</li>
	 * <li>Verify the device's DHCP server address.</li>
	 * <li>Verify device's software operation status.</li>
	 * 
	 * @author Revanth Kumar Vella
	 * @Refactor Alan_Bivera
	 *           </ol>
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1026", testDecription = "Verify Ethernet Related SNMP")
	public void verifyEthernetSnmp(Dut device) {

		// Variable Declaration begins
		String testCaseId = null;
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		// Variable Declation Ends

		testCaseId = "TC-RDK-SNMP-126";
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1026");
		LOGGER.info("TEST DESCRIPTION: Verify Ethernet Related SNMP");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify ethernet port link speed using SNMP MIB.");
		LOGGER.info("2. Verify the Date using SNMP MIB .");
		LOGGER.info("3. Verify device's maximum CEP using SNMP MIB.");
		LOGGER.info("4. Verify DHCP server type of device using SNMP MIB.");
		LOGGER.info("5. Verify the device's DHCP server address.");
		LOGGER.info("6. Verify device's software operation status.");

		LOGGER.info("#######################################################################################");

		try {
			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			errorMessage = "Failed to get ethernet port link speed using SNMP MIB(1.3.6.1.2.1.2.2.1.5.1).";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify the ethernet port link speed using SNMP MIB.");
			LOGGER.info(
					"STEP 1: ACTION : Get the field value for ethernet port link speed using SNMP MIB(1.3.6.1.2.1.2.2.1.5.1).");
			LOGGER.info(
					"STEP 1: EXPECTED : Ethernet port link speed should return value of Integer like 0(0 Mbps), 10000000(10 Mbps), 100000000(100 Mbps) or 1000000000(1000Mbps).");
			LOGGER.info("**********************************************************************************");
			try {
				response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_CM_ETHERNET_OPER_SETTING.getOid(),
						BroadBandSnmpMib.ECM_CM_ETHERNET_OPER_SETTING.getTableIndex());
				if (CommonMethods.isNotNull(response)
						&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
						&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))) {
					// Validate whether the Eternet port link speed returned is valid.
					status = BroadBandSnmpUtils.validateCmEthernetOperSettingValueRetrievedFromSnmp(response);
				} else {
					errorMessage = "Obtained null response/No such oid for  snmp mib execution to get ethernet port speed";
					LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
				}
				if (status) {
					LOGGER.info("STEP 1: ACTUAL : SUCCESSFULLY VERIFIED ETHERNET PORT LINK SPEED AS " + response);
				} else {
					errorMessage = "Expected ethernet port link speed value should be "
							+ "like 0(0 Mbps), 10000000(10 Mbps), 100000000(100 Mbps) or 1000000000(1000Mbps). But obtained response is : "
							+ response;
					LOGGER.error("STEP 1: ACTUAL :" + errorMessage);
				}
			} catch (TestException exception) {
				errorMessage = "Exception occured while geting Ethernet port link speed : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "s2";
			errorMessage = "Failed to get Date using SNMP MIB(.1.3.6.1.2.1.69.1.1.2.0).";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify the Date using SNMP MIB .");
			LOGGER.info("STEP 2: ACTION : Get the field value for Date using SNMP MIB (.1.3.6.1.2.1.69.1.1.2.0).");
			LOGGER.info(
					"STEP 2: EXPECTED : A valid Date should be displayed. It should return value of type Hex-STRING.");
			LOGGER.info("**********************************************************************************");
			try {
				response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_DATE.getOid(), BroadBandSnmpMib.ECM_DATE.getTableIndex());
				status = CommonMethods.isNotNull(response)
						&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
						&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))
						&& !BroadBandCommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)
						&& CommonMethods.patternMatcher(response, BroadBandTestConstants.HEXA_STRING_REGEX);
				if (status) {
					LOGGER.info(
							"STEP 2: ACTUAL : SUCCESSFULLY OBTAINED FIELD VALUE FOR DATE USING SNMP MIB(.1.3.6.1.2.1.69.1.1.2.0) : "
									+ response);
				} else {
					errorMessage = "Obtained null response/No such oid for  snmp mib execution to get Date";
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} catch (TestException exception) {
				errorMessage = "Exception occured while geting Date value : " + exception.getMessage();
				LOGGER.error(errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			}

			stepNum = "s3";
			errorMessage = "Failed to get device's maximum CEP using SNMP MIB (.1.3.6.1.2.1.69.1.1.7.0).";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify device's maximum CEP using SNMP MIB.");
			LOGGER.info(
					"STEP 3: ACTION : Get the field value for maximum CEP using SNMP MIB  (.1.3.6.1.2.1.69.1.1.7.0).");
			LOGGER.info(
					"STEP 3: EXPECTED : Device's maximum CEP should return value of type integer value like 1 or 5.");
			LOGGER.info("**********************************************************************************");
			try {
				long startTime = System.currentTimeMillis();
				do {
					response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
							BroadBandSnmpMib.ECM_DEV_MAX_CPE.getOid(),
							BroadBandSnmpMib.ECM_DEV_MAX_CPE.getTableIndex());
					if (CommonMethods.isNotNull(response)) {
						// initially response is compared between static values 1 and 5,now it is
						// modified as
						// comparison between snmp and webpa response.
						errorMessage = "Expected Device's maximum CEP value using snmp and webpa are not same";
						status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_TO_CHECK_CPE_VALUE, response);
					}
				} while (!status
						&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.ONE_MINUTE_IN_MILLIS
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
				if (status) {
					LOGGER.info("STEP 3: ACTUAL : SUCCESSFULLY VERIFIED THE FIELD VALUE FOR MAXIMUM CEP OF DEVICE AS "
							+ response);
				} else {
					LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			} catch (TestException exception) {
				errorMessage = "Exception occured while geting Device's maximum CEP : " + exception.getMessage();
				LOGGER.error(errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			}

			stepNum = "s4";
			errorMessage = "Failed to get DHCP server type of device using SNMP MIB(.1.3.6.1.2.1.69.1.4.6.0).";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify DHCP server type of device using SNMP MIB.");
			LOGGER.info(
					"STEP 4: ACTION : Get the field value for DHCP server type using SNMP MIB  (.1.3.6.1.2.1.69.1.4.6.0).");
			LOGGER.info(
					"STEP 4: EXPECTED : A valid DHCP server type should be displayed. It should return value of type integer 2.");
			LOGGER.info("**********************************************************************************");
			try {
				response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_DEV_SERVER_DHCP_TYPE.getOid(),
						BroadBandSnmpMib.ECM_DEV_SERVER_DHCP_TYPE.getTableIndex());
				if (CommonMethods.isNotNull(response)) {
					// Validate whether the DHCP server type returned is valid.
					status = response.equals(BroadBandTestConstants.STRING_VALUE_TWO);
					if (status) {
						LOGGER.info("STEP 4: ACTUAL : SUCCESSFULLY VERIFIED VALUE OF DEVICE'S DHCP SERVER TYPE AS "
								+ response);
					} else {
						errorMessage = "Expected DHCP server type should be " + "2. But obtained response is : "
								+ response;
						LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
					}
				} else {
					errorMessage = "Obtained null response/No such oid for snmp mib execution to get DHCP server type";
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			} catch (TestException exception) {
				errorMessage = "Exception occured while geting DHCP server type : " + exception.getMessage();
				LOGGER.error(errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			}

			stepNum = "s5";
			errorMessage = "Failed to get device's DHCP server address using SNMP MIB( .1.3.6.1.2.1.69.1.4.7.0).";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify the device's DHCP server address.");
			LOGGER.info(
					"STEP 5: ACTION : Get the field value for DHCP server address using SNMP MIB( .1.3.6.1.2.1.69.1.4.7.0).");
			LOGGER.info("STEP 5: EXPECTED : A DHCP server address should be displayed.");
			LOGGER.info("**********************************************************************************");
			try {
				response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_DEV_SERVER_DHCP_ADDRESS.getOid(),
						BroadBandSnmpMib.ECM_DEV_SERVER_DHCP_ADDRESS.getTableIndex());
				status = CommonMethods.isNotNull(response)
						&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
						&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))
						&& !BroadBandCommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)
						&& CommonMethods.patternMatcher(response, BroadBandTestConstants.HEXA_STRING_REGEX);
				if (status) {
					LOGGER.info(
							"STEP 5: ACTUAL : SUCCESSFULLY OBTAINED FIELD VALUE FOR DHCP SERVER ADDRESS USING SNMP MIB( .1.3.6.1.2.1.69.1.4.7.0) : "
									+ response);
				} else {
					errorMessage = "Obtained null response/No such oid for  snmp mib execution to get DHCP server address";
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			} catch (TestException exception) {
				errorMessage = "Exception occured while geting DHCP server address : " + exception.getMessage();
				LOGGER.error(errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			}

			stepNum = "s6";
			errorMessage = "Failed to get device's operation status value using SNMP MIB(.1.3.6.1.2.1.69.1.3.4.0).";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify device's software operation status value.");
			LOGGER.info(
					"STEP 6: ACTION : Get the field value for software operation status using SNMP MIB (.1.3.6.1.2.1.69.1.3.4.0).");
			LOGGER.info(
					"STEP 6: EXPECTED : A valid software operation status should be displayed. It should return value of type integer value like 1, 2, 3, 4 or 5.");
			LOGGER.info("**********************************************************************************");
			try {
				response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_DEV_SW_OPER_STATUS.getOid(),
						BroadBandSnmpMib.ECM_DEV_SW_OPER_STATUS.getTableIndex());
				if (CommonMethods.isNotNull(response)) {
					// Validate whether the DevSwOperStatus returned is valid.
					status = response.equals(BroadBandTestConstants.STRING_VALUE_ONE)
							|| response.equals(BroadBandTestConstants.STRING_VALUE_TWO)
							|| response.equals(BroadBandTestConstants.STRING_VALUE_THREE)
							|| response.equals(BroadBandTestConstants.STRING_VALUE_FOUR)
							|| response.equals(BroadBandTestConstants.STRING_VALUE_FIVE);
					if (status) {
						LOGGER.info(
								"STEP 6: ACTUAL : SUCCESSFULLY VERIFIED FIELD VALUE FOR SOFTWARE OPERATION STATUS AS "
										+ response);
					} else {
						errorMessage = "Expected software operation status should be like 3 or 5. But obtained response is : "
								+ response;
						LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
					}
				} else {
					errorMessage = "Obtained null response/No such oid for  snmp mib execution to get software operation status";
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			} catch (TestException exception) {
				errorMessage = "Exception occured while geting software operation status : " + exception.getMessage();
				LOGGER.error(errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			}
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1026");
	}

	/**
	 * Test to validate that the Delegated IPv6 prefix can be retrieved using
	 * rdkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)
	 * 
	 * Step 1 : Get the delegated IPv6 prefix of the device using SNMP get
	 * 
	 * Step 2: Validate the IP returned in the above response is in line with the
	 * device logs
	 * 
	 * Step 3: Validate the IP returned in the above response is in line with the
	 * TR-181 parameters
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Sathurya Ravi
	 * @Refactor Alan_Bivera
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1025", testDecription = "Validate that the Delegated IPv6 prefix can be retrieved using  rdkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)")
	public void testValidateStaticIPv6Address(Dut device) {
		// stores the test case id
		String testCaseId = "TC-RDKB-SNMP-125";
		// stores the step number
		int stepNumber = 1;
		// stores the test step number
		String step = "S" + stepNumber;
		// stores the test result
		boolean status = false;
		// stores the error message
		String errorMessage = "";
		// stores the command response
		String response = "";
		// stores staticIpAddress
		String staticIpAddress = "";
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1025");
			LOGGER.info(
					"TEST DESCRIPTION: Validate that the Delegated IPv6 prefix can be retrieved using  rdkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("Step 1. Get the delegated IPv6 prefix of the device using SNMP get");
			LOGGER.info("Step 2. Validate the IP returned in the above response is in line with the device logs ");
			LOGGER.info(
					"Step 3. Validate the IP returned in the above response is in line with the TR-181 parameters ");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1 : Get the delegated IPv6 prefix of the device using SNMP get
			 */
			stepNumber = 1;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION: Get the delegated IPv6 prefix of the device using SNMP get ");
			LOGGER.info("STEP " + stepNumber
					+ ":ACTION: Excecute SNMP get on the MIB  rdkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: The SNMP get response should return the delegated IPv6 prefix of the device");
			LOGGER.info("**********************************************************************************");
			errorMessage = "The device has returned some Junk value for the dkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)";
			// Execute SNMP get operation for the MIB
			// dkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV6.getOid(),
					BroadBandSnmpMib.ECM_STATIC_WAN_IPV6.getTableIndex());
			LOGGER.info("SNMP command output for dkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0) : "
					+ response);
			if (CommonMethods.isNotNull(response)) {
				// Validate whether the IP address returned is valid.
				status = CommonMethods.isIpv6Address(response.substring(0, response.indexOf("/")));
				staticIpAddress = response;
				errorMessage = "The IP address returned is not in a proper format. Actual: " + response;

			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: SNMP get response returned the delegated IPV6 prefix of the device" + response);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage + response);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

			/**
			 * Step 2 : Validate the IP returned in the above response is in line with the
			 * device logs
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION: Validate the IP returned in the above response is in line with the device logs ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Parse through the output of the command cat /etc/dibbler/server.conf by SSHing to the device and get the static WAN IP.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: The parsed Static IP value and the value from the SNMP response should be the same");
			LOGGER.info("**********************************************************************************");
			errorMessage = "The delegated IPv6 prefix from the device logs is invalid";
			response = tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.COMMAND_TO_GET_IPV6_PREFIX_VALUE_ZEBRA);
			status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response, staticIpAddress);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: The parsed Static IP value and the value from the SNMP is same");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

			/**
			 * Step 3 : Validate the IP returned in the above response is in line with the
			 * TR-181 parameters
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION: Validate the IP returned in the above response is in line with the TR-181 parameters ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Parse through the output of the command dmcli eRT getv Device.IP.Interface.1.IPv6Prefix.1.Prefix and get the value of the delegated IPv6 prefix.");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: The parsed Static IP value and the value from the SNMP response should be the same");
			LOGGER.info("**********************************************************************************");
			errorMessage = "The delegated IPv6 prefix from the TR-181 is invalid";
			// Execute the command dmcli eRT getv
			// Device.IP.Interface.1.IPv6Prefix.1.Prefix by SSHing to the device and
			// get the static WAN IP address
			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DELEGATED_IPV6_PREFIX);
			status = response.equalsIgnoreCase(staticIpAddress);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: The parsed Static IP value and the value from the SNMP response is same");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		} catch (Exception exception) {
			status = false;
			errorMessage = "Unable to validate that the delegated IPv6 prefix can be retrieved using  rdkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.7.0)"
					+ exception.getMessage();
			LOGGER.error(errorMessage);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1025");
	}

	/**
	 * Verify the cable modem SNMPv3 parameters from RDK using RFC/XConf (Broadcom
	 * RDKB platforms)
	 * <ol>
	 * <li>Verify default value for SNMPV2 support using TR-181 parameters</li>
	 * <li>Verify default value for SNMPV3 support using TR-181 parameters</li>
	 * <li>Verify default value of Snmpv3DHKickstart enabled status</li>
	 * <li>Verify default value of TableNumberOfEntries for Snmpv3DHKickstart</li>
	 * <li>Enable SNMPv3 using TR-181 parameters through WebPA/Dmcli</li>
	 * <li>Configure SNMPv3 related security parameters and security numbers in
	 * RFC</li>
	 * <li>Verify SNMPV3 support using TR-181 parameters</li>
	 * <li>Verify Snmpv3DHKickstart enabled status</li>
	 * <li>Verify TableNumberOfEntries for Snmpv3DHKickstart</li>
	 * <li>Verify SNMPv3 security name values using TR-181 parameters</li>
	 * <li>Verify SNMPv3 security number values using TR-181 parameters</li>
	 * <li>Verify snmpv3 security parameters enabled via rfc logs from dcmrfc.log
	 * file</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Gnanaprakasham S
	 * @refactor Govardhan
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-SNMP-1027")
	public void verifyCableModelSnmpv3ParameterConfigurationViaRfc(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-SNMP-127";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1027");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the cable modem SNMPv3 parameters from RDK using RFC/XConf (Broadcom RDKB platforms)");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify default value for SNMPV2 support using TR-181 parameters");
		LOGGER.info("2. Verify default value for SNMPV3 support using TR-181 parameters");
		LOGGER.info("3. Verify default value of Snmpv3DHKickstart enabled status");
		LOGGER.info("4. Verify default value of TableNumberOfEntries for Snmpv3DHKickstart");
		LOGGER.info("5. Enable SNMPv3 using TR-181 parameters through WebPA/Dmcli");
		LOGGER.info("6. Configure SNMPv3 related security parameters and security numbers in RFC");
		LOGGER.info("7. Verify SNMPV3 support using TR-181 parameters");
		LOGGER.info("8. Verify Snmpv3DHKickstart enabled status");
		LOGGER.info("9. Verify TableNumberOfEntries for Snmpv3DHKickstart");
		LOGGER.info("10. Verify SNMPv3 security name values using TR-181 parameters");
		LOGGER.info("11. Verify SNMPv3 security number values using TR-181 parameters");
		LOGGER.info("12. Verify snmpv3 security parameters enabled via rfc logs from dcmrfc.log file");

		LOGGER.info("#######################################################################################");

		try {

			List<String> parameters = new ArrayList<String>();

			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_2_SUPPORT);
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_SUPPORT);
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART);
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_TABLE_NUMBER);

			String[] parametersArray = new String[parameters.size()];
			parametersArray = parameters.toArray(parametersArray);

			Map<String, String> deviceStatusResponse = BroadBandWebPaUtils
					.getMultipleParameterValuesUsingWebPaOrDmcli(device, tapEnv, parametersArray);

			// In some case SNMPv3 may be configured via RFC rule. So in this scenario we
			// can skip the step for checking
			// default value for snmpv3
			response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_RFC_VALUE_CONFIG_FILE);
			boolean isSnmpv3EnabledByRfc = false;

			if (CommonMethods.isNotNull(response)) {
				response = CommonMethods.patternFinder(response,
						"tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support\\W+(\\w+)");
				if (CommonMethods.isNotNull(response)) {
					isSnmpv3EnabledByRfc = Boolean.parseBoolean(response.trim());
					LOGGER.info("SNMPv3 is enabled by RFC configuration : " + isSnmpv3EnabledByRfc);
				}
			}

			stepNum = "s1";
			errorMessage = "SNMPv2 support is not available/enabled by default";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify default value for SNMPV2 support using TR-181 parameters");
			LOGGER.info(
					"STEP 1: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V2Support\" parameter through WebPA/Dmcli for eg : dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V2Support");
			LOGGER.info(
					"STEP 1: EXPECTED : Default value of SNMPv2 support must be enabled and the value of TR-181 parameter should be true");
			LOGGER.info("**********************************************************************************");

			response = deviceStatusResponse.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_2_SUPPORT);
			status = CommonMethods.isNotNull(response) && Boolean.parseBoolean(response);

			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : SUCCESSFULLY VERIFIED SNMPV2 IS ENABLED BY DEFAULT USING Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V2Support PARAMETER");
			} else {
				LOGGER.error(
						"STEP 1: ACTUAL : snmpv2 is not enabled by default. current enabled status is : " + status);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Failed to verify SNMPv3 supported status using TR-181 parameter";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify default value for SNMPV3 support using TR-181 parameters");
			LOGGER.info(
					"STEP 2: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support\" parameter through WebPA/Dmcli for eg : dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support");
			LOGGER.info(
					"STEP 2: EXPECTED : Default value of SNMPv3 support must be disabled and the value of TR-181 parameter should be false (if snmpv3 enabled In RFC rule, value can be true)");
			LOGGER.info("**********************************************************************************");

			if (!isSnmpv3EnabledByRfc) {
				response = deviceStatusResponse.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_SUPPORT);
				status = CommonMethods.isNotNull(response) && !Boolean.parseBoolean(response);

				if (status) {
					LOGGER.info(
							"STEP 2: ACTUAL : SUCCESSFULLY VERIFIED SNMPV3 IS DISABLED BY DEFAULT USING Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support PARAMETER");
				} else {
					errorMessage = "snmpv3 is not disabled by default. current enabled status is : " + status;
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			} else {
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						"Snmpv3 is enabled via RFC. So step 2 is not applicable", false);
			}

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Snmpv3DHKickstart is not disabled by default";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify default value of Snmpv3DHKickstart enabled status");
			LOGGER.info(
					"STEP 3: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Enabled\" parameter through WebPA/Dmcli for eg : dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Enabled");
			LOGGER.info(
					"STEP 3: EXPECTED : Snmpv3DHKickstart must be disabled by default and the value of TR-181 parameter should be false");
			LOGGER.info("**********************************************************************************");

			response = deviceStatusResponse.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART);
			status = CommonMethods.isNotNull(response) && Boolean.parseBoolean(response);

			if (!status) {
				LOGGER.info(
						"STEP 3: ACTUAL : SUCCESSFULLY VERIFIED Snmpv3DHKickstart IS DISABLED BY DEFAULT USING Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Enabled PARAMETER");
			} else {
				errorMessage = "Snmpv3DHKickstart is not disabled by default. current enabled status is : " + status;
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, !status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Failed to verify TableNumberOfEntries as 5 for Snmpv3DHKickstart";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify default value of TableNumberOfEntries for Snmpv3DHKickstart");
			LOGGER.info(
					"STEP 4: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.TableNumberOfEntries\" parameter through WebPA/Dmcli for eg : dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.TableNumberOfEntries");
			LOGGER.info("STEP 4: EXPECTED : TableNumberOfEntries value must be 5 by default");
			LOGGER.info("**********************************************************************************");

			response = deviceStatusResponse
					.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_TABLE_NUMBER);
			status = CommonMethods.isNotNull(response) && response.equals(BroadBandTestConstants.STRING_VALUE_5);

			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : SUCCESSFULLY VERIFIED TABLE NUMBER OF ENTRIES FOR Snmpv3DHKickstart IS 5");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage + " obtained value is : " + response);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "Failed to enabled SNMPv3 support";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Enable SNMPv3 using TR-181 parameters through WebPA/Dmcli");
			LOGGER.info(
					"STEP 5: ACTION : Set \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support\" parameter value as true for eg : dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support bool true");
			LOGGER.info("STEP 5: EXPECTED : Snmpv3 must be enabled");
			LOGGER.info("**********************************************************************************");

			if (!isSnmpv3EnabledByRfc) {

				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_SUPPORT, WebPaDataTypes.BOOLEAN.getValue(),
						BroadBandTestConstants.TRUE);

				if (status) {
					LOGGER.info(
							"STEP 5: ACTUAL : SUCCESSFULLY CONFIGURED Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support PARAMETER VALUE AS TRUE ");
				} else {
					errorMessage = "Failed to configure Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support parameter value as true";
					LOGGER.error("STEP 5: ACTUAL :  " + errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			} else {
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						"Snmpv3 is enabled via RFC. So step 5 is not applicable", false);
			}

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "Failed to configure SNMPv3 related security parameters in RFC";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Configure SNMPv3 related security parameters and security numbers in RFC");
			LOGGER.info(
					"STEP 6: ACTION : Configure the below paramaters with corresponding values in RFCDevice.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.EnabledDevice.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Table.{i}.SecurityName  Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Table.{1}.SecurityNumber");
			LOGGER.info("STEP 6: EXPECTED : Snmpv3 related security parameters must be configured in RFC");
			LOGGER.info("**********************************************************************************");

			try {
				status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
						BroadBandTestConstants.SNMPV3_DH_KICKSTART_TABLE_RFC_FEATURE, true);
			} catch (Exception exception) {
				errorMessage = "Exception occurred while enabling rfc feature : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : SUCCESSFULLY CONFIGURED CABLE MODEM SNMPV3 PARAMETERS IN XCONF VIA RFC");
			} else {
				LOGGER.error("STEP 6: ACTUAL : Failed to configure cable modem snmpv3 parameters in xconf via rfc ");
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s7";
			errorMessage = "Failed to enabled SNMPv3 support using Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify SNMPV3 support using TR-181 parameters");
			LOGGER.info(
					"STEP 7: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support\" parameter through WebPA/Dmcli for eg : dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support");
			LOGGER.info(
					"STEP 7: EXPECTED : SNMPv3 support must be enabled and the value of TR-181 parameter should be true");
			LOGGER.info("**********************************************************************************");

			deviceStatusResponse = BroadBandWebPaUtils.getMultipleParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					parametersArray);

			response = deviceStatusResponse.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_SUPPORT);
			status = CommonMethods.isNotNull(response) && Boolean.parseBoolean(response);

			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : SUCCESSFULLY VERIFIED SNMPV3 IS ENABLED AFTER REBOOT USING Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support PARAMETER");
			} else {
				LOGGER.error("STEP 7: ACTUAL : SNMPV3 is not enabled after reboot");
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "Failed to enable Snmpv3DHKickstart using Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Enabled";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify Snmpv3DHKickstart enabled status");
			LOGGER.info(
					"STEP 8: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Enabled\" parameter through WebPA/Dmcli for eg : dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Enabled");
			LOGGER.info(
					"STEP 8: EXPECTED : Snmpv3DHKickstart must be enabled and the value of TR-181 parameter should be true");
			LOGGER.info("**********************************************************************************");

			response = deviceStatusResponse.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART);
			status = CommonMethods.isNotNull(response) && Boolean.parseBoolean(response);

			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL : SUCCESSFULLY VERIFIED Snmpv3DHKickstart IS ENABLED AFTER REBOOT USING Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Enabled PARAMETER");
			} else {
				LOGGER.error(
						"STEP 8: ACTUAL : Snmpv3DHKickstart is not enabled after reboot current enabled status is : "
								+ status);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "Failed to verify TableNumberOfEntries for Snmpv3DHKickstart";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify TableNumberOfEntries for Snmpv3DHKickstart");
			LOGGER.info(
					"STEP 9: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.TableNumberOfEntries\" parameter through WebPA/Dmcli for eg : dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.TableNumberOfEntries");
			LOGGER.info("STEP 9: EXPECTED : TableNumberOfEntries value must be 5 by default");
			LOGGER.info("**********************************************************************************");

			response = deviceStatusResponse
					.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_TABLE_NUMBER);
			status = CommonMethods.isNotNull(response) && response.equals(BroadBandTestConstants.STRING_VALUE_5);

			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : SUCCESSFULLY VERIFIED TABLE NUMBER OF ENTRIES FOR Snmpv3DHKickstart IS 5");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage + " obtained response is : " + response);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "Failed to configure the snmpv3 security name through RFC settings";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify Snmpv3DHKickstart security name values using TR-181 parameters");
			LOGGER.info(
					"STEP 10: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Table.{i}.SecurityName\" parameters via webpa/dmcli and verify values");
			LOGGER.info(
					"STEP 10: EXPECTED : Snmpv3DHKickstart security name values must be same as value configured in RFC settings");
			LOGGER.info("**********************************************************************************");

			parameters.clear();
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NAME
					.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_ONE));
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NAME
					.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_TWO));
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NAME
					.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_THREE));
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NAME
					.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_FOUR));
			parametersArray = new String[parameters.size()];
			parametersArray = parameters.toArray(parametersArray);

			deviceStatusResponse = BroadBandWebPaUtils.getMultipleParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					parametersArray);
			List<String> list = Collections
					.unmodifiableList(Arrays.asList(BroadBandTestConstants.SNMPV3_DH_KICK_START_SECURITY_NAME_1,
							BroadBandTestConstants.SNMPV3_DH_KICK_START_SECURITY_NAME_2,
							BroadBandTestConstants.SNMPV3_DH_KICK_START_SECURITY_NAME_3,
							BroadBandTestConstants.SNMPV3_DH_KICK_START_SECURITY_NAME_4));
			boolean validation = true;

			for (int iteration = 0; iteration < parameters.size(); iteration++) {

				response = deviceStatusResponse
						.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NAME
								.replace(BroadBandTestConstants.TR181_NODE_REF, String.valueOf(iteration + 1)));
				LOGGER.info("Obtained security name is : " + response);

				if (CommonMethods.isNotNull(response)) {
					status = response.equals(list.get(iteration));

					if (status) {
						LOGGER.info(
								"Successfully verified \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Table."
										+ iteration + 1 + ".SecurityName values is " + list.get(iteration));
					} else {
						validation = false;
						LOGGER.error(
								"Snmpv3DHKickstart security name is not configurable by RFC configuration. Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Table."
										+ iteration + 1 + ".SecurityName configured as " + list.get(iteration)
										+ " rfc. but after obtained value frm device is " + response);
					}
				}
			}

			if (!validation) {
				status = validation;
			}

			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : SUCCESSFULLY VERIFIED Snmpv3DHKickstart SECURITY NAME IS CONFIGURABLE VIA RFC");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			errorMessage = "Failed to configure the snmpv3 security number through RFC settings";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 11: DESCRIPTION : Verify Snmpv3DHKickstart security number values using TR-181 parameters");
			LOGGER.info(
					"STEP 11: ACTION : Execute \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Table.{i}.SecurityNumber\" parameters via webpa/dmcli and verify values");
			LOGGER.info(
					"STEP 11: EXPECTED : Snmpv3DHKickstart security number values must be same as value configured in RFC settings");
			LOGGER.info("**********************************************************************************");

			parameters.clear();
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NUMBER
					.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_ONE));
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NUMBER
					.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_TWO));
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NUMBER
					.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_THREE));
			parameters.add(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NUMBER
					.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_FOUR));
			parametersArray = new String[parameters.size()];
			parametersArray = parameters.toArray(parametersArray);

			deviceStatusResponse = BroadBandWebPaUtils.getMultipleParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					parametersArray);

			List<String> securityNumberlist = Collections
					.unmodifiableList(Arrays.asList(AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_SNMPV3_DH_KICK_START_SECURITY_NUMBER_1),
							AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_SNMPV3_DH_KICK_START_SECURITY_NUMBER_2),
							AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_SNMPV3_DH_KICK_START_SECURITY_NUMBER_3),
							AutomaticsPropertyUtility
							.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_SNMPV3_DH_KICK_START_SECURITY_NUMBER_4)));

			validation = true;

			for (int iteration = 0; iteration < parameters.size(); iteration++) {

				response = deviceStatusResponse
						.get(BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NUMBER
								.replace(BroadBandTestConstants.TR181_NODE_REF, String.valueOf(iteration + 1)));
				LOGGER.info("Obtained security number is : " + response);

				if (CommonMethods.isNotNull(response)) {
					status = response.equals(securityNumberlist.get(iteration));

					if (status) {
						LOGGER.info(
								"Successfully verified \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Table."
										+ iteration + 1 + ".SecurityName values is "
										+ securityNumberlist.get(iteration));
					} else {
						validation = false;
						LOGGER.error(
								"Snmpv3DHKickstart security number is not configurable by RFC configuration. Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.Snmpv3DHKickstart.Table."
										+ iteration + 1 + ".SecurityNumber configured as "
										+ securityNumberlist.get(iteration)
										+ " rfc. but after obtained value from device is " + response);
					}
				}
			}

			if (!validation) {
				status = validation;
			}

			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL : SUCCESSFULLY VERIFIED Snmpv3DHKickstart SECURITY NUMBER IS CONFIGURABLE VIA RFC");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s12";
			errorMessage = "Failed to verify snmpv3 security parameters enabled via rfc logs from dcmrfc.log file.";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 12: DESCRIPTION : Verify snmpv3 security parameters enabled via rfc logs from dcmrfc.log file");
			LOGGER.info(
					"STEP 12: ACTION : Execute \"cat dcmrfc.log | grep -inr \"snmp\"\" command and verify rfc logs");
			LOGGER.info(
					"STEP 12: EXPECTED : snmpv3 security parameters enabled via rfc logs logs must be present in dcmrfc.log file");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.CMD_TO_GET_SNMPV3_DH_KICKSTART_LOGS);

			if (CommonMethods.isNotNull(response)) {

				status = response
						.contains(BroadBandTestConstants.TR181_DOT
								+ BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART)
						&& response.contains(BroadBandTestConstants.TR181_DOT
								+ (BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NAME
										.replace(BroadBandTestConstants.TR181_NODE_REF,
												BroadBandTestConstants.STRING_VALUE_ONE)))
						&& response.contains(BroadBandTestConstants.TR181_DOT
								+ (BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_DH_KICKSTART_SECURITY_NUMBER
										.replace(BroadBandTestConstants.TR181_NODE_REF,
												BroadBandTestConstants.STRING_VALUE_ONE)));

			} else {
				errorMessage = "Obtained null response. Failed to get the logs related to snmpv3 security parameters configurable via rfc logs from dcmrfc.log file. ";
			}

			if (status) {
				LOGGER.info(
						"STEP 12: ACTUAL : SUCCESSFULLY VERIFIED SNMPV3 SECURITY PARAMETERS ENABLED VIA RFC LOGS FROM dcmrfc.log FILE");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			status = false;
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Disabled SNMPV3 and removed rfc override from device");
			LOGGER.info(
					"POST-CONDITION : ACTION : 1)set snmp as v2 2) Disable snmpv3 using TR-181 parameter 3) remove rfc override 4) reboot");
			LOGGER.info("POST-CONDITION : EXPECTED : Disabled SNMPv3 successfully");

			try {
				System.setProperty(BroadBandTestConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V2.toString());
				BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_SUPPORT, WebPaDataTypes.BOOLEAN.getValue(),
						BroadBandTestConstants.FALSE);
				tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_REMOVE_RFC_OVERRIDE);
				tapEnv.executeCommandUsingSsh(device, "cp /etc/rfc.properties /nvram/");
				status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

				if (!status) {
					LOGGER.error("Device is not accessible after reboot");
				} else {

					response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_SNMP_VERSION_3_SUPPORT);
					status = CommonMethods.isNotNull(response) && !Boolean.parseBoolean(response);

					if (status) {
						LOGGER.info("Successfully verified deivce is disbled with snmpv3 after test case execution");
					}
				}

				tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_REMOVE_RFC_OVERRIDE);

			} catch (Exception exception) {
				LOGGER.info("Exception occurred in post condition :" + exception.getMessage());
			}

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1027");
	}

	/**
	 * Test to verify ITG DOCS MIB OID
	 * 
	 * Test verifies DocsisEventCount,DocsisEventText,DocsDevServerBootState,
	 * DocsIfCmStatusTxPower,DocsIfDownChannelPower,DocsIfSigQSignalNoise
	 * 
	 * @author Karthick Pandiyan
	 * @refactor anandam
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1007", testDecription = "Verify the Current software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0)")
	public void testToVerifyITGDocsMIB(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-007";
		List<String> match = new ArrayList<String>();
		String errorMessage = null;
		String stepNumber = "s1";
		boolean status = false;
		try {

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1007");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the Current software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0)");
			LOGGER.info("1. Verify DocsisEventCount (1.3.6.1.2.1.69.1.5.8.1.4) SNMP OID");
			LOGGER.info("2. Verify DocsisEventText (1.3.6.1.2.1.69.1.5.8.1.7) SNMP OID");
			LOGGER.info("3. Verify DocsDevServerBootState (1.3.6.1.2.1.69.1.4.1) SNMP OID");
			LOGGER.info("4. Verify DocsIfCmStatusTxPower (1.3.6.1.2.1.10.127.1.2.2.1.3) SNMP OID");
			LOGGER.info("5. Verify DocsIfDownChannelPower (1.3.6.1.2.1.10.127.1.1.1.1.6) SNMP OID");
			LOGGER.info("6. Verify DocsIfSigQSignalNoise (1.3.6.1.2.1.10.127.1.1.4.1.5) SNMP OID");
			LOGGER.info("#######################################################################################");

			errorMessage = "Failed to verify DOCSIS Event Count";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: Verify DocsisEventCount (1.3.6.1.2.1.69.1.5.8.1.4) SNMP OID ");
			LOGGER.info(
					"EXPECTED: Should check all the returned value mibs, all values should be integer and more than 0");
			LOGGER.info("**********************************************************************************");
			// Do snmpwalk on docsis event count
			String snmpCommandOutput = BroadBandSnmpUtils.snmpWalkOnEcm(tapEnv, device,
					BroadBandSnmpMib.ESTB_DOCS_IS_EVENT_COUNT.getOid());
			match = BroadBandCommonUtils.patternFinderForMultipleMatches(snmpCommandOutput,
					BroadBandTestConstants.DOCSIS_EVENT_COUNT_PATTERN);
			LOGGER.info("Matched string " + match);
			int eventCount = match.size();
			status = BroadBandCommonUtils.compareListWithLimitValue(match, 1);
			if (!status) {
				LOGGER.error(errorMessage);
			}
			LOGGER.info("ACTUAL: Verification of DOCSIS Event Count " + status);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			stepNumber = "s2";
			status = false;
			errorMessage = "Failed to verify DOCSIS Event text";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: Verify DocsisEventText (1.3.6.1.2.1.69.1.5.8.1.7) SNMP OID ");
			LOGGER.info("EXPECTED: Corresponding text should be available for each event in event count");
			LOGGER.info("**********************************************************************************");
			status = BroadBandSnmpUtils.verifyDocsisEventText(tapEnv, device, eventCount);
			if (!status) {
				LOGGER.error(errorMessage);
			}
			LOGGER.info("ACTUAL: Verification of DOCSIS Event text : " + status);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			stepNumber = "s3";
			status = false;
			errorMessage = "Failed to verify DOCS Dev Server Boot State";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: Verify DocsDevServerBootState (1.3.6.1.2.1.69.1.4.1) SNMP OID ");
			LOGGER.info("EXPECTED: Should return boot state value and it should be operational( 1 )");
			LOGGER.info("**********************************************************************************");
			status = BroadBandSnmpUtils.verifyDocsDevServerBootState(tapEnv, device);
			if (!status) {
				LOGGER.error(errorMessage);
			}
			LOGGER.info("ACTUAL: Verification of DOCS Dev Server Boot State : " + status);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			stepNumber = "s4";
			status = false;
			errorMessage = "Failed to verify DOCSIF CM status TX power";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: Verify DocsIfCmStatusTxPower (1.3.6.1.2.1.10.127.1.2.2.1.3) SNMP OID ");
			LOGGER.info("EXPECTED: DocsifCmStatusTxPower value should be in between 80 to 580");
			LOGGER.info("**********************************************************************************");
			status = BroadBandSnmpUtils.verifyDocsIfCmStatusTxPower(tapEnv, device);
			if (!status) {
				LOGGER.error(errorMessage);
			}
			LOGGER.info("ACTUAL: Verification of DOCSIF CM status TX power : " + status);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			stepNumber = "s5";
			status = false;
			errorMessage = "Failed to verify DOCSIF Down Channel Power";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: Verify DocsIfDownChannelPower (1.3.6.1.2.1.10.127.1.1.1.1.6) SNMP OID ");
			LOGGER.info("EXPECTED: DocsIfDownChannelPower value should be in between -150 to 150");
			LOGGER.info("**********************************************************************************");
			status = BroadBandSnmpUtils.verifyDocsIfDownChannelPower(tapEnv, device);
			if (!status) {
				LOGGER.error(errorMessage);
			}
			LOGGER.info("ACTUAL: Verification of DOCSIF Down Channel Power : " + status);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			stepNumber = "s6";
			status = false;
			errorMessage = "Failed to verify DOCSIF SigQSignal Noise";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: Verify DocsIfSigQSignalNoise (1.3.6.1.2.1.10.127.1.1.4.1.5) SNMP OID ");
			LOGGER.info("EXPECTED: DocsIfSigQSignalNoise OID return value should be more than 120");
			LOGGER.info("**********************************************************************************");
			status = BroadBandSnmpUtils.verifyDocsIfSigQSignalNoise(tapEnv, device);
			if (!status) {
				LOGGER.error(errorMessage);
			}
			LOGGER.info("ACTUAL: Verification of DOCSIF SigQSignal Noise : " + status);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			status = false;
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		}
	}

	/**
	 * Verify Access Points security mode via SNMP parameter
	 * <ol>
	 * <li>STEP 1: Verify Retrieving the Security mode of access point via WEBPA
	 * Parameter</li>
	 * <li>STEP 2: Verify Retrieving the Security mode of access point via SNMP
	 * Parameter and compare with WEBPA response</li>
	 * <li>STEP 3: Repeat step 1-2 for remaining 15 accesspoints(Total number of
	 * steps-32)</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Deepika Sekar
	 * @refactor anandam
	 *
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-SNMP-5050", testDecription = "Verify Access Points security mode via snmp parameter")
	public void testToVerifySnmpSecurityMode(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-SNMP-550";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		String webPaResponse = null;
		String webPaParam = null;
		String snmpResponse = null;
		String replaceValue = null;
		int internalIteration = 1;
		int stepNumber = 0;
		int webPaSecurityModevalue = 0;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-5050");
		LOGGER.info(
				"TEST DESCRIPTION: Verify system description, Modem Configuration Filename,EMTA Address,Cable Interface MAC Address,Serial Number via SNMP and cross validate with WEBPA");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify Retrieving the Security mode of access point via WEBPA Parameter");
		LOGGER.info(
				"2. Verify Retrieving the Security mode of access point via SNMP Parameter and compare with webpa Response");
		LOGGER.info("3. Repeat step 1-3 for remaining 15 accesspoints(Total Number of steps-32)");

		LOGGER.info("#######################################################################################");

		try {
			for (int iteration = BroadBandTestConstants.CONSTANT_1; iteration <= BroadBandTestConstants.CONSTANT_16; iteration++) {
				if (iteration != BroadBandTestConstants.CONSTANT_2 && iteration != BroadBandTestConstants.CONSTANT_3
						&& iteration != BroadBandTestConstants.CONSTANT_5
						&& iteration != BroadBandTestConstants.CONSTANT_11
						&& iteration != BroadBandTestConstants.CONSTANT_13) {
					stepNumber++;
					stepNum = "S" + stepNumber;
					errorMessage = "Unable to get security mode from  WebPa response";
					status = false;
					if (iteration <= BroadBandTestConstants.CONSTANT_8) {

						replaceValue = BroadBandSnmpConstants.SNMP_COMMAND_2_4_GHZ_SECURITY_MODE
								.replace(BroadBandTestConstants.STRING_REPLACE, String.valueOf(iteration));

					} else {
						if (internalIteration != BroadBandTestConstants.CONSTANT_3
								&& internalIteration != BroadBandTestConstants.CONSTANT_5) {
							replaceValue = BroadBandSnmpConstants.SNMP_COMMAND_5_GHZ_SECURITY_MODE
									.replace(BroadBandTestConstants.STRING_REPLACE, String.valueOf(internalIteration));
							internalIteration++;
						} else {
							replaceValue = BroadBandSnmpConstants.SNMP_COMMAND_5_GHZ_SECURITY_MODE.replace(
									BroadBandTestConstants.STRING_REPLACE, String.valueOf(++internalIteration));
							internalIteration++;
						}
					}

					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP " + stepNumber
							+ ": DESCRIPTION : Verify Retrieving the Security mode of access point via WEBPA Parameter:Device.WiFi.AccessPoint.2.Security.ModeEnabled");
					LOGGER.info("STEP " + stepNumber + ": ACTION : \"Execute dmcli  command : "
							+ BroadBandWebPaConstants.WEBPA_PARAM_SECURITY_MODE_OF_WIFI
									.replace(BroadBandTestConstants.TR181_NODE_REF, replaceValue));
					LOGGER.info("STEP " + stepNumber
							+ ": EXPECTED : Security mode of access point  is retrieve via WEBPA ");
					LOGGER.info("**********************************************************************************");

					webPaParam = BroadBandWebPaConstants.WEBPA_PARAM_SECURITY_MODE_OF_WIFI
							.replace(BroadBandTestConstants.TR181_NODE_REF, replaceValue);

					webPaResponse = tapEnv.executeWebPaCommand(device, webPaParam);
					status = CommonMethods.isNotNull(webPaResponse);
					if (status) {
						webPaSecurityModevalue = BroadBandTestConstants.MAP_SECURITY_MODE_AND_KEY_TO_GET_POSSIBLE_SECURITY_MODE_VALUES
								.get(webPaResponse);
						LOGGER.info("STEP " + stepNumber
								+ ": ACTUAL : Security mode value retieved successfully from WEBPA param "
								+ webPaParam);
					} else {
						LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
					}
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

					stepNumber++;
					stepNum = "S" + stepNumber;
					errorMessage = "Unable to get security mode from  snmp response";
					status = false;

					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP " + stepNumber
							+ ": DESCRIPTION : Verify Retrieving the Security mode of access point via SNMP Parameter:Device.WiFi.AccessPoint.2.Security.ModeEnabled");
					LOGGER.info("STEP " + stepNumber + ": ACTION : \"Execute SNMP  command : "
							+ BroadBandSnmpMib.ECM_WIFI_RADIO_MODE_2_4.getOid());
					LOGGER.info("STEP " + stepNumber + ": EXPECTED : SNMP command executed successfully");
					LOGGER.info("**********************************************************************************");

					snmpResponse = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
							BroadBandSnmpMib.ECM_WIFI_RADIO_MODE_2_4.getOid(), replaceValue);
					LOGGER.error("snmpresponse : " + snmpResponse);
					if (CommonMethods.isNotNull(snmpResponse)) {
						status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, snmpResponse,
								String.valueOf(webPaSecurityModevalue));
					}
					if (status) {
						LOGGER.info("STEP " + stepNumber
								+ ": ACTUAL : Security mode value retieved successfully from SNMP param with response "
								+ snmpResponse);
					} else {
						LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage + "  " + snmpResponse);
					}

					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

				}
			}
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Unable to get security mode from  snmp response";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Retrieving the Security mode of access point via WEBPA Parameter:Device.WiFi.AccessPoint.2.Security.ModeEnabled");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : \"Execute dmcli  command : Device.WiFi.AccessPoint.10003.Security.ModeEnabled");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Security mode of access point  is retrieve via WEBPA ");
			LOGGER.info("**********************************************************************************");

			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PUBLIC_SECURITY_MODEENABLED);
			status = CommonMethods.isNotNull(webPaResponse);
			if (status) {

				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Security mode value retieved successfully from WEBPA param " + webPaParam);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Unable to get security mode from  snmp response";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Retrieving the Security mode of access point via WEBPA Parameter:Device.WiFi.AccessPoint.2.Security.ModeEnabled");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : \"Execute dmcli  command : Device.WiFi.AccessPoint.10103.Security.ModeEnabled");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Security mode of access point  is retrieve via WEBPA ");
			LOGGER.info("**********************************************************************************");

			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PUBLIC_SECURITY_MODEENABLED);
			status = CommonMethods.isNotNull(webPaResponse);
			if (status) {

				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Security mode value retieved successfully from WEBPA param " + webPaParam);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Unable to get security mode from  snmp response";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Retrieving the Security mode of access point via WEBPA Parameter:Device.WiFi.AccessPoint.2.Security.ModeEnabled");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : \"Execute dmcli  command : Device.WiFi.AccessPoint.10005.Security.ModeEnabled");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Security mode of access point  is retrieve via WEBPA ");
			LOGGER.info("**********************************************************************************");

			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_SECURED_PUBLICWIFI);
			status = CommonMethods.isNotNull(webPaResponse);
			if (status) {

				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Security mode value retieved successfully from WEBPA param " + webPaParam);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Unable to get security mode from  snmp response";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Retrieving the Security mode of access point via WEBPA Parameter:Device.WiFi.AccessPoint.2.Security.ModeEnabled");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : \"Execute dmcli  command : Device.WiFi.AccessPoint.10105.Security.ModeEnabled");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Security mode of access point  is retrieve via WEBPA ");
			LOGGER.info("**********************************************************************************");

			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SECURED_PUBLICWIFI);
			status = CommonMethods.isNotNull(webPaResponse);
			if (status) {

				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Security mode value retieved successfully from WEBPA param " + webPaParam);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info(" ENDING TEST CASE: TC-RDKB-SNMP-5050");
	}

	/**
	 * Test to verify enabling/disabling bridge mode through SNMP v3 is successful
	 * 
	 * <ol>
	 * <li>PRE CONDITION 1: Disable codebig first enable using webpa</li>
	 * <li>STEP 1: Enable SNMPv3 support using RFC</li>
	 * <li>STEP 2: Verify SNMPv3 support parameter is enabled in dcmrfc.log
	 * file</li>
	 * <li>STEP 3: Verify SNMPv3 support is enabled using webpa</li>
	 * <li>STEP 4:Execute SNMP v3 SET command on
	 * OID(1.3.6.1.4.1.17270.50.2.3.2.1.1.32) with set value as 1 to enable bridge
	 * mode and retrieve the lan mode via webpa and cross check whether the bridge
	 * mode is enabled</li>
	 * <li>STEP 5: Execute SNMP v3 SET command on
	 * OID(1.3.6.1.4.1.17270.50.2.3.2.1.1.32) with set value as 2 to disablebridge
	 * mode and retrieve the lan mode via webpa and cross check whether the bridge
	 * mode is disabled</li>
	 * <li>STEP 6: Execute SNMP v3 SET command on
	 * OID(1.3.6.1.4.1.31621.1.1.1.1.1.1.40) with set value as 2 to disable MoCA and
	 * retrieve the MoCA Interface status via webpa and cross check whether the MoCA
	 * is disabled</li>
	 * <li>STEP 7: Execute SNMP v3 SET command on
	 * OID(1.3.6.1.4.1.31621.1.1.1.1.1.1.40) with set value as 1 to enable MoCA and
	 * retrieve the MoCA Interface status via webpa and cross check whether the MoCA
	 * is enabled</li>
	 * <li>STEP 8:Execute SNMP v3 GET command on OID
	 * (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001) to get the value of 2.4 GHz private
	 * Wi-Fi SSID</li>
	 * <li>STEP 9:Execute SNMP v3 GET command on OID
	 * (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101) to get the value of 5 GHz private
	 * Wi-Fi SSID</li>
	 * <li>STEP 10: Execute TR181 Device.WiFi.SSID.10003.SSID to get the value of
	 * 2.4 GHz public Wi-Fi SSID</li>
	 * <li>STEP 11: Execute TR181 Device.WiFi.SSID.10103.SSID to get the value of 5
	 * GHz public Wi-Fi SSID</li>
	 * <li>STEP 12: Execute SNMP v3 GET command on OID
	 * (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10001) to get the value of 2.4 GHz private
	 * Wi-Fi Passphrase</li>
	 * <li>STEP 13:Execute SNMP v3 GET command on OID
	 * (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101) to get the value of 5 GHz private
	 * Wi-Fi Passphrase</li>
	 * <li>STEP 14: Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.2.0) to get the value of number of Ping Per
	 * Server</li>
	 * <li>STEP 15: Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.3.0) to get the value of minimum number of Ping
	 * Server</li>
	 * <li>STEP 16: Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.4.0) to get the value of Ping Interval</li>
	 * <li>STEP 17:Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.5.0) to get the value of Ping response wait
	 * time</li>
	 * <li>STEP 18:Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.7.0) to get the value of Resource Usage Compute
	 * Window</li>
	 * <li>STEP 19: Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.8.0) to get the average value of CPU Threshold</li>
	 * <li>STEP 20: Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.9.0) to get the average value of Memory
	 * Threshold</li>
	 * <li>STEP 21:Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.10.0) to get the value of maximum Reboot Count</li>
	 * <li>STEP 22: Execute SNMP v3 WALK command on OID
	 * (1.3.6.1.4.1.17270.44.1.1.11.0) to get the value of maximum Subsystem Reset
	 * Count</li>
	 * <li>STEP 23: Execute SNMP v3 SET command on OID (1.3.6.1.2.1.1.1.0) which is
	 * a READ-ONLY MIB</li>
	 * <li>STEP 24: Execute SNMP v3 SET command on OID
	 * (.1.3.6.1.4.1.17270.44.1.1.4.0) with incorrect value out of expected
	 * range(Expected Range is between 15 and 1440, both inclusive)</li>
	 * <li>POST-CONDITION 1: Set the device back to router mode if device is in
	 * Bridge mode</li>
	 * <li>POST-CONDITION 2: Revert back the moca value</li>
	 * <li>POST-CONDITION 3: Verify disabling the public wifi using webpa</li>
	 * </ol>
	 * 
	 * @param device The device to be used.
	 * @refactor anandam
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-SNMP-1012")
	public void testSnmpV3EnableDisableBridgeMode(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-112";
		String stepNumber = "s1";
		boolean status = false; // stores the test status
		String errorMessage = null; // stores the error message
		String webpaOutput = null; // stores Webpa output
		String snmpOutput = null; // stores SNMP output
		String response = null;
		boolean isMocaEnabled = false;
		boolean isPublicWifiEnabled = false;
		int stepNum = 1;
		boolean isBridgeModeDisabled = false;
		BroadBandResultObject result = null; // stores test result and error message
		ExecutionStatus testStatus = ExecutionStatus.FAILED; // stores the execution status
		String systemCommand = null;
		boolean isSNMPv3Enabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_SNMPV3_SUPPORT, BroadBandTestConstants.TRUE,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		boolean isBussinessDevice = DeviceModeHandler.isBusinessClassDevice(device);
		boolean isFibreDevice = DeviceModeHandler.isFibreDevice(device);
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE :TC-RDKB-SNMP-1012");
			LOGGER.info("TEST DESCRIPTION: Test to Verify enabling/disabling parameters through SNMP v3 is successful");
			LOGGER.info("PRE CONDITION 1: Disable codebig first enable using webpa");
			LOGGER.info("STEP 1: Enable SNMPv3 support using RFC");
			LOGGER.info("STEP 2: Verify SNMPv3 support parameter is enabled in dcmrfc.log file");
			LOGGER.info("STEP 3: Verify SNMPv3 support is enabled using webpa");
			LOGGER.info(
					"STEP 4:Execute SNMP v3 SET command on OID(1.3.6.1.4.1.17270.50.2.3.2.1.1.32) with set value as 1 to enable bridge mode and retrieve the lan mode via webpa and cross check whether the bridge mode is enabled");
			LOGGER.info(
					"STEP 5:  Execute SNMP v3 SET command on OID(1.3.6.1.4.1.17270.50.2.3.2.1.1.32) with set value as 2 to disablebridge mode and retrieve the lan mode via webpa and cross check whether the bridge mode is disabled");
			LOGGER.info(
					"STEP 6: Execute SNMP v3  SET command on OID(1.3.6.1.4.1.31621.1.1.1.1.1.1.40) with set value as 2 to disable MoCA and retrieve the MoCA Interface status via webpa and cross check  whether the MoCA is disabled ");
			LOGGER.info(
					"STEP 7: Execute SNMP v3  SET command on OID(1.3.6.1.4.1.31621.1.1.1.1.1.1.40) with set value as 1  to enable MoCA and retrieve the MoCA Interface status via webpa and cross check whether the MoCA is enabled");
			LOGGER.info(
					"STEP 8:Execute SNMP v3  GET command on OID (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001)  to get the value of  2.4 GHz private Wi-Fi SSID");
			LOGGER.info(
					"STEP 9:Execute SNMP v3 GET command on OID (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101) to get the value of 5 GHz private Wi-Fi SSID");
			LOGGER.info(
					"STEP 10: Execute TR181 Device.WiFi.SSID.10003.SSID to get the value of 2.4 GHz public Wi-Fi SSID");
			LOGGER.info(
					"STEP 11: Execute TR181 Device.WiFi.SSID.10103.SSID to get the value of 5 GHz public Wi-Fi SSID");
			LOGGER.info(
					"STEP 12: Execute SNMP v3 GET command on OID(1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10002) to get the value of 2.4 GHz public Home SSID");
			LOGGER.info(
					"STEP 13: SNMP v3  GET command on OID(1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10102) to get the value of 5 GHz public Home SSID");
			LOGGER.info(
					"STEP 14: Execute SNMP v3 GET command on OID (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10001) to get the value of  2.4 GHz private Wi-Fi Passphrase");
			LOGGER.info(
					"STEP 15:Execute SNMP v3 GET command on OID (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101) to get the value of 5 GHz private Wi-Fi Passphrase");
			LOGGER.info(
					"STEP 16:  Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.2.0) to get the value of number of Ping Per Server");
			LOGGER.info(
					"STEP 17: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.3.0) to get the value of minimum number of Ping Server ");
			LOGGER.info(
					"STEP 18: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.4.0) to get the value of Ping Interval ");
			LOGGER.info(
					"STEP 19:Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.5.0) to get the value of Ping response wait time");
			LOGGER.info(
					"STEP 20:Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.7.0) to get the value of  Resource Usage Compute Window");
			LOGGER.info(
					"STEP 21: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.8.0) to get the average value of CPU Threshold");
			LOGGER.info(
					"STEP 22: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.9.0) to get the average value of Memory Threshold");
			LOGGER.info(
					"STEP 23:Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.10.0) to get the value of maximum Reboot Count");
			LOGGER.info(
					"STEP 24: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.11.0) to get the value of maximum Subsystem Reset Count");
			LOGGER.info("STEP 25: Execute SNMP v3 SET command on OID (1.3.6.1.2.1.1.1.0) which is a READ-ONLY MIB");
			LOGGER.info(
					"STEP 26: Execute SNMP v3 SET command on OID (.1.3.6.1.4.1.17270.44.1.1.4.0) with incorrect value out of expected range(Expected Range is between 15 and 1440, both inclusive)");
			LOGGER.info("POST-CONDITION 1: Set the device back to router mode if device is in Bridge mode");
			LOGGER.info("POST-CONDITION 2: Revert back the moca value");
			LOGGER.info("POST-CONDITION 3: Verify disabling the public wifi using webpa");
			LOGGER.info("POST-CONDITION 4: Verify disabling XHS SSID using webpa");
			LOGGER.info("#######################################################################################");

			LOGGER.info("################################# STARTING PRE-CONFIGURATIONS #############################");
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");
			if (!isSNMPv3Enabled) {

				stepNumber = "S" + stepNum;
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Enable SNMPv3 support using RFC");
				LOGGER.info("STEP " + stepNum + ": ACTION: 1. update RFC server url 2. post payload data 3. reboot");
				LOGGER.info("STEP " + stepNum + ": EXPECTED: Device rebooted after posting rfc profile successfully");
				LOGGER.info("******************************************************************************");

				errorMessage = "Failed to enable SNMPv3 support using RFC";
				status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
						BroadBandTestConstants.SNMPV3, true);
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: Successfully posted the payload data to enable SNMPv3 support & rebooted the device to get the latest RFC configuration ");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

				stepNum++;
				stepNumber = "S" + stepNum;
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNum
						+ ": DESCRIPTION: Verify SNMPv3 support parameter is enabled in dcmrfc.log file");
				LOGGER.info("STEP " + stepNum
						+ ": ACTION: Execute command: grep -i \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support value=true\" /rdklogs/logs/dcmrfc.log");
				LOGGER.info("STEP " + stepNum + ": EXPECTED: enabled the SNMPv3 support using RFC");
				LOGGER.info("******************************************************************************");
				errorMessage = "Failed to get log message of SNMPv3 support value = true in dcmrfc.log file";
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_SNMPV3_SUPPORT, BroadBandCommandConstants.FILE_DCMRFC_LOG,
						BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				status = CommonMethods.isNotNull(response)
						&& CommonUtils.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.TRUE);
				if (status) {
					LOGGER.info("STEP " + stepNum + ": ACTUAL: Successfully enabled the SNMPv3 support using RFC");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

				stepNum++;
				stepNumber = "S" + stepNum;
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Verify SNMPv3 support is enabled using webpa ");
				LOGGER.info("STEP " + stepNum
						+ ": ACTION: Execute webpa command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support");
				LOGGER.info("STEP " + stepNum + ": EXPECTED: Webpa get request is success and parameter value is true");
				LOGGER.info("******************************************************************************");
				errorMessage = "Failed to get the response for webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support";

				response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_SNMPV3_SUPPORT);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: Successfully verified the SNMPv3 support is enabled using webpa operation");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				errorMessage = "Snmpv3 is by default enabled. So step 1-3 is not applicable as SNMPv3 is already enabled in the device ";
				stepNum = 1;
				while (stepNum <= 3) {
					stepNumber = "s" + stepNum;
					errorMessage += ",Marked as Not Applicable";
					LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNum++;
				}
			}
			/**
			 * Step 4 : Execute SNMP v3 SET command on
			 * OID(1.3.6.1.4.1.17270.50.2.3.2.1.1.32) with set value as 1 to enable bridge
			 * mode and retrieve the lan mode via webpa and cross check whether the bridge
			 * mode is enabled
			 * 
			 */
			stepNum = BroadBandTestConstants.CONSTANT_4;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Enable bridge mode via SNMPv3 and retrieve the lan mode via webpa and cross check  whether the bridge mode is enabled ");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3  SET command on OID(1.3.6.1.4.1.17270.50.2.3.2.1.1.32) with set value as 1 to enable bridge mode and retrieve the lan mode via webpa and cross check  whether the bridge mode is enabled ");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: on executing, SNMP should return the Integer 1 and  lan mode via webpa should return 'bridge-static'");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			snmpOutput = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_ONE,
					BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex());
			status = CommonMethods.isNotNull(snmpOutput) && snmpOutput.equals(BroadBandTestConstants.STRING_VALUE_ONE);
			errorMessage = "Bridge mode can not be enabled using SNMP v3 Query";
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 90 SECONDS.");
				tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
				errorMessage = "Unable to verify the WebPA process is up after setting the Lan Mode to 'Bridge-Static'";
				status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				// In case the 8 minutes wait for WebPA Process to be up is not sufficient,
				// performing the check again.
				if (!status) {
					status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				}
			}
			if (status) {
				errorMessage = "Unable to retrieve the Lan Mode using WebPA.";
				// Retrieve the lan mode via webpa and cross check whether the bridge mode is
				// enabled
				webpaOutput = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS);
				status = CommonMethods.isNotNull(webpaOutput)
						&& webpaOutput.equalsIgnoreCase(BroadBandTestConstants.LAN_MANAGEMENT_MODE_BRIDGE_STATIC);
				errorMessage = "Unable to verify the Lan Mode using WebPA. Expected: Bridge-Static, Actual: "
						+ webpaOutput;
			}
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Bridge mode is enabled using SNMP v3 Query & retrieved the lan mode via webpa and verified, Bridge mode is enabled");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 5 : Execute SNMP v3 SET command on
			 * OID(1.3.6.1.4.1.17270.50.2.3.2.1.1.32) with set value as 2 to disable bridge
			 * mode and retrieve the lan mode via webpa and cross check whether the bridge
			 * mode is disabled
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Disable bridge mode via SNMPv3 and retrieve the lan mode via webpa and cross check whether the bridge mode is disabled");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute SNMP v3  SET command on OID(1.3.6.1.4.1.17270.50.2.3.2.1.1.32) with set value as 2 to disable bridge mode nd retrieve the lan mode via webpa and cross check whether the bridge mode is disabled");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: on executing, SNMP should return the Integer 1 and  lan mode via webpa should return 'router'");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			snmpOutput = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_TWO,
					BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex());
			status = CommonMethods.isNotNull(snmpOutput) && snmpOutput.equals(BroadBandTestConstants.STRING_VALUE_TWO);
			errorMessage = "Bridge mode can not be disabled using SNMP v3 Query";
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 90 SECONDS.");
				tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
				errorMessage = "Unable to verify the WebPA process is up after setting the Lan Mode to 'Router'";
				status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				// In case the 8 minutes wait for WebPA Process to be up is not sufficient,
				// performing the check again.
				if (!status) {
					status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				}
			}
			if (status) {
				errorMessage = "Unable to retrieve the Lan Mode using WebPA.";
				// Retrieve the lan mode via webpa and cross check whether the bridge mode is
				// enabled
				webpaOutput = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS);
				status = CommonMethods.isNotNull(webpaOutput)
						&& webpaOutput.equalsIgnoreCase(BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER);
				isBridgeModeDisabled = status;
				errorMessage = "Unable to verify the Lan Mode using WebPA. Expected: Router, Actual: " + webpaOutput;
			}
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Bridge mode is disabled using SNMP v3 Query & retrieved the lan mode via webpa and verified, Bridge mode is disabled");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 6: Execute SNMP v3 SET command on OID(1.3.6.1.4.1.31621.1.1.1.1.1.1.40)
			 * with set value as 2 to disable MoCA and retrieve the MoCA Interface status
			 * via webpa and cross check whether the MoCA is disabled
			 * 
			 */
			if (!isBussinessDevice) {
				stepNum++;
				stepNumber = "S" + stepNum;
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNum
						+ ": DESCRIPTION: Disable MoCA via SNMPv3and retrieve the MoCA Interface status via webpa and cross check  whether the MoCA is disabled ");
				LOGGER.info("STEP " + stepNum
						+ ": ACTION: Execute SNMP v3  SET command on OID(1.3.6.1.4.1.31621.1.1.1.1.1.1.40) with set value as 2 to disable MoCA and retrieve the MoCA Interface status via webpa and cross check  whether the MoCA is disabled ");
				LOGGER.info("STEP " + stepNum
						+ ": EXPECTED: on executing, SNMP should return the Integer value 1 and MoCA Interface status via Webpa should return 'false'");
				LOGGER.info("**********************************************************************************");
				System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
				snmpOutput = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv,
						BroadBandSnmpMib.ESTB_VERIFY_MOCA_INTERFACE_STATUS.getOid(), SnmpDataType.INTEGER,
						BroadBandTestConstants.STRING_VALUE_TWO, BroadBandTestConstants.STRING_VALUE_FORTY);
				status = CommonMethods.isNotNull(snmpOutput)
						&& snmpOutput.equals(BroadBandTestConstants.STRING_VALUE_TWO);
				errorMessage = "MoCA can not be disabled using SNMP v3 Query";
				if (status) {
					tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
					// Retrieve the moca status via webpa and cross check whether the MoCA is
					// disabled
					webpaOutput = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_MOCA_INTERFACE_ENABLE);
					status = CommonMethods.isNotNull(webpaOutput)
							&& webpaOutput.equalsIgnoreCase(BroadBandTestConstants.FALSE);
					errorMessage = (!status && CommonMethods.isNotNull(webpaOutput)
							&& webpaOutput.equalsIgnoreCase(BroadBandTestConstants.TRUE))
									? "Retrieved the MoCA status via webpa and verified, MoCA can not be disabled using SNMP v3"
									: "Checking the MoCA state via webpa failed, MoCA status can not be verified";
				}
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: MoCA is disabled using SNMP v3 Query & retrieved the MoCA status via webpa and verified, MoCA is disabled");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

				/**
				 * Step 7: Execute SNMP v3 SET command on OID(1.3.6.1.4.1.31621.1.1.1.1.1.1.40)
				 * with set value as 1 to enable MoCA and retrieve the MoCA Interface status via
				 * webpa and cross check whether the MoCA is enabled
				 * 
				 */
				stepNum++;
				stepNumber = "S" + stepNum;
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNum
						+ ": DESCRIPTION: Enable MoCA via SNMPv3and retrieve the MoCA Interface status via webpa and cross check whether the MoCA is enabled");
				LOGGER.info("STEP " + stepNum
						+ ": ACTION: Execute SNMP v3  SET command on OID(1.3.6.1.4.1.31621.1.1.1.1.1.1.40) with set value as 1  to enable MoCA and retrieve the MoCA Interface status via webpa and cross check whether the MoCA is enabled");
				LOGGER.info("STEP " + stepNum
						+ ": EXPECTED: on executing, SNMP should return the Integer value 1 and MoCA Interface status via Webpa should return 'true'");
				LOGGER.info("**********************************************************************************");
				System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
				snmpOutput = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv,
						BroadBandSnmpMib.ESTB_VERIFY_MOCA_INTERFACE_STATUS.getOid(), SnmpDataType.INTEGER,
						BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_VALUE_FORTY);
				status = CommonMethods.isNotNull(snmpOutput)
						&& snmpOutput.equals(BroadBandTestConstants.STRING_VALUE_ONE);
				errorMessage = "MoCA can not be enabled using SNMP v3 Query";
				if (status) {
					tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
					// Retrieve the moca status via webpa and cross check whether the MoCA is
					// enabled
					webpaOutput = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_MOCA_INTERFACE_ENABLE);
					status = CommonMethods.isNotNull(webpaOutput)
							&& webpaOutput.equalsIgnoreCase(BroadBandTestConstants.TRUE);
					isMocaEnabled = status;
					errorMessage = (!status && CommonMethods.isNotNull(webpaOutput)
							&& webpaOutput.equalsIgnoreCase(BroadBandTestConstants.FALSE))
									? "Retrieved the MoCA status via webpa and verified, MoCA can not be enabled using SNMP v3"
									: "Checking the MoCA state via webpa failed, MoCA status can not be verified";
				}
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: MoCA is enabled using SNMP v3 Query & retrieved the MoCA status via webpa and verified, MoCA is enabled");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				errorMessage = "Moca is not applicable for Business models ";
				stepNum = 6;
				while (stepNum <= 7) {
					stepNumber = "s" + stepNum;
					errorMessage += ",Marked as Not Applicable";
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNum++;
				}
			}
			/**
			 * Step 8: Execute SNMP v3 GET command on OID
			 * (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001) to get the value of 2.4 GHz private
			 * Wi-Fi SSID
			 * 
			 */
			stepNum = BroadBandTestConstants.CONSTANT_8;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Retieve 2.4 GHz private Wi-Fi SSID using SNMPv3");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3  GET command on OID (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001)  to get the value of  2.4 GHz private Wi-Fi SSID");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: The private Wi-Fi SSID of 2.4GHz Radio should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			systemCommand = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv, BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandTestConstants.PRIVATE_WIFI_TYPE, BroadBandTestConstants.SSID_PARAM);
			result = BroadBandSnmpUtils.retrieveSnmpOutputAndVerifyWithSystemCommandOutput(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getTableIndex(), systemCommand,
					"private Wi-Fi SSID of 2.4GHz");
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			testStatus = result.getExecutionStatus();
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: The private Wi-Fi SSID of 2.4GHz Radio can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 9 : Execute SNMP v3 GET command on OID
			 * (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101) to get the value of 5 GHz private
			 * Wi-Fi SSID
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			testStatus = ExecutionStatus.FAILED;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Retieve value of 5 GHz private Wi-Fi SSID using SNMPv3");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 GET command on OID (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101) to get the value of 5 GHz private Wi-Fi SSID");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: The private Wi-Fi SSID of 5GHz Radio should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			systemCommand = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv, BroadBandTestConstants.BAND_5GHZ,
					BroadBandTestConstants.PRIVATE_WIFI_TYPE, BroadBandTestConstants.SSID_PARAM);
			result = BroadBandSnmpUtils.retrieveSnmpOutputAndVerifyWithSystemCommandOutput(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getTableIndex(), systemCommand,
					"private Wi-Fi SSID of 5GHz");
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			testStatus = result.getExecutionStatus();
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: The private Wi-Fi SSID of 5 GHz Radio can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 10 : Execute TR181 Device.WiFi.SSID.10003.SSID to get the value of 2.4
			 * GHz public Wi-Fi SSID
			 * 
			 */
			if (!isFibreDevice) {
				stepNum++;
				stepNumber = "S" + stepNum;
				status = false;
				testStatus = ExecutionStatus.FAILED;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Retrieve public Wi-Fi 2.4Ghz SSID using webpa");
				LOGGER.info("STEP " + stepNum
						+ ": ACTION: Execute TR181 Device.WiFi.SSID.10003.SSID to get the value of 2.4 GHz public Wi-Fi SSID");
				LOGGER.info("STEP " + stepNum
						+ ": EXPECTED: The public Wi-Fi SSID of 2.4GHz Radio should be retrieved using webpa Query");
				LOGGER.info("**********************************************************************************");
				System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
				// Enable 2.4 GHz public Wifi
				LOGGER.info("### Enabling 2.4 GHz public Wi-Fi ###");
				status = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS)
						&& BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS);
				errorMessage = "Unable to check 2.4 Ghz SSID of public Wi-Fi using SNMP v3, since public Wi-Fi can not be enabled";
				testStatus = ExecutionStatus.NOT_TESTED;
				isPublicWifiEnabled = status;
				if (status) {
					status = false;
					String ssidRetrievedFromSystemCommand = null;
					tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
					systemCommand = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
							BroadBandTestConstants.BAND_2_4GHZ, BroadBandTestConstants.PUBLIC_WIFI_TYPE,
							BroadBandTestConstants.SSID_PARAM);
					ssidRetrievedFromSystemCommand = BroadBandWebPaUtils.getSsidNameRetrievedUsingDeviceCommand(device,
							tapEnv, systemCommand);
					status = CommonMethods.isNotNull(ssidRetrievedFromSystemCommand);
					errorMessage = "Using system command unable to retrieve " + BroadBandTestConstants.SSID_PARAM
							+ " Radio";
					if (status) {

						status = false;
						String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID);
						LOGGER.info("2.4GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
						status = CommonMethods.isNotNull(ssidNameFromWebPa) && CommonUtils
								.patternSearchFromTargetString(ssidRetrievedFromSystemCommand, ssidNameFromWebPa);
						testStatus = status ? ExecutionStatus.PASSED : ExecutionStatus.FAILED;
						errorMessage = "webpa response failed for Device.WiFi.SSID.10003.SSID :" + ssidNameFromWebPa;
					}
				}
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: The public Wi-Fi SSID of 2.4GHz Radio can be retrieved using webpa");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

				/**
				 * Step 11 : Execute TR181 Device.WiFi.SSID.10103.SSID to get the value of 5 GHz
				 * public Wi-Fi SSID
				 * 
				 */
				stepNum++;
				stepNumber = "S" + stepNum;
				status = false;
				testStatus = ExecutionStatus.FAILED;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Retrieve value of 5 GHz public Wi-Fi SSID using webpa");
				LOGGER.info("STEP " + stepNum
						+ ": ACTION: Execute TR181 Device.WiFi.SSID.10103.SSID to get the value of 5 GHz public Wi-Fi SSID");
				LOGGER.info("STEP " + stepNum
						+ ": EXPECTED: The public Wi-Fi SSID of 5GHz Radio should be retrieved using webpa Query");
				LOGGER.info("**********************************************************************************");
				System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
				// Enable 5 GHz public Wifi
				LOGGER.info("### Enabling 5 GHz public Wi-Fi ###");
				status = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS)
						&& BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS);
				errorMessage = "Unable to check 5 Ghz SSID of public Wi-Fi using SNMP v3, since public Wi-Fi can not be enabled";
				testStatus = ExecutionStatus.NOT_TESTED;
				isPublicWifiEnabled = (isPublicWifiEnabled) ? isPublicWifiEnabled : status;
				if (status) {
					status = false;
					String ssidRetrievedFromSystemCommand = null;
					tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
					systemCommand = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv,
							BroadBandTestConstants.BAND_5GHZ, BroadBandTestConstants.PUBLIC_WIFI_TYPE,
							BroadBandTestConstants.SSID_PARAM);
					ssidRetrievedFromSystemCommand = BroadBandWebPaUtils.getSsidNameRetrievedUsingDeviceCommand(device,
							tapEnv, systemCommand);
					status = CommonMethods.isNotNull(ssidRetrievedFromSystemCommand);
					errorMessage = "Using system command unable to retrieve " + BroadBandTestConstants.SSID_PARAM
							+ " Radio";
					if (status) {
						status = false;
						String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID);
						LOGGER.info("5GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
						status = CommonMethods.isNotNull(ssidNameFromWebPa) && CommonUtils
								.patternSearchFromTargetString(ssidRetrievedFromSystemCommand, ssidNameFromWebPa);
						testStatus = status ? ExecutionStatus.PASSED : ExecutionStatus.FAILED;
						errorMessage = "Using webpa unable to retrieve "
								+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID + " Radio:"
								+ ssidNameFromWebPa;
					}
				}
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: The public Wi-Fi SSID of 5 GHz Radio can be retrieved using webpa");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);
			} else {
				errorMessage = "public wifi is not applicable for fibre models ";
				stepNum = 10;
				while (stepNum <= 11) {
					stepNumber = "s" + stepNum;
					errorMessage += ",Marked as Not Applicable";
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNum++;
				}
			}

			/**
			 * Step 12 : Execute SNMP v3 GET command on OID
			 * (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10001) to get the value of 2.4 GHz private
			 * Wi-Fi Passphrase
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			testStatus = ExecutionStatus.FAILED;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Retrieve the value of  2.4 GHz private Wi-Fi Passphrase using SNMPv3");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 GET command on OID (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10001) to get the value of  2.4 GHz private Wi-Fi Passphrase");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: The private Wi-Fi Passphrase of 2.4GHz Radio should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			result = BroadBandSnmpUtils.retrieveSnmpOutputAndVerifyWithWebPaOutput(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getTableIndex(),
					BroadBandWebPaConstants.WEBPA_WAREHOUSE_WIRELESS_SSID_PASSWORD_PRIVATE_2G,
					"private Wi-Fi Passphrase of 2.4GHz");
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			testStatus = result.getExecutionStatus();
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: The private Wi-Fi Passphrase of 2.4GHz Radio can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 13 : Execute SNMP v3 GET command on OID
			 * (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101) to get the value of 5 GHz private
			 * Wi-Fi Passphrase
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			testStatus = ExecutionStatus.FAILED;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNum + ": DESCRIPTION: Retrieve value of 5 GHz private Wi-Fi Passphrase using SNMPv3");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 GET command on OID (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101) to get the value of 5 GHz private Wi-Fi Passphrase");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: The private Wi-Fi Passphrase of 5GHz Radio should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			result = BroadBandSnmpUtils.retrieveSnmpOutputAndVerifyWithWebPaOutput(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_PASSPHRASE.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_PASSPHRASE.getTableIndex(),
					BroadBandWebPaConstants.WEBPA_WAREHOUSE_WIRELESS_SSID_PASSWORD_PRIVATE_5G,
					"private Wi-Fi Passphrase of 2.4GHz");
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			testStatus = result.getExecutionStatus();
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: The private Wi-Fi Passphrase of 5 GHz Radio can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 14 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.2.0)
			 * to get the value of number of Ping Per Server
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.2.0) to get the value of number of Ping Per Server");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.2.0) to get the value of number of Ping Per Server");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: Value of no. of ping per server should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_NO_OF_PINGS_PER_SERVER.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim().equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
			errorMessage = "Value of no. of ping per server can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Value of no. of ping per server can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 15 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.3.0)
			 * to get the value of minimum number of Ping Server
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.3.0) to get the value of minimum number of Ping Server");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.3.0) to get the value of minimum number of Ping Server");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: Value of minimum number of Ping Server should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_MIN_NO_OF_PING_SERVER.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim().equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
			errorMessage = "Value of minimum number of Ping Server can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Value of minimum number of Ping Server can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 16 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.4.0)
			 * to get the value of Ping Interval
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.4.0) to get the value of Ping Interval ");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.4.0) to get the value of Ping Interval");
			LOGGER.info(
					"STEP " + stepNum + ": EXPECTED: Value of Ping Interval should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_PING_INTERVAL.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim()
							.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_DEFAULT_PING_INTERVAL);
			errorMessage = "Value of Ping Interval can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info(
						"STEP " + stepNum + ": ACTUAL: Value of Ping Interval can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 17 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.5.0)
			 * to get the value of Ping response wait time
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.5.0) to get the value of Ping response wait time");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.5.0) to get the value of Ping response wait time");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: Value of Ping response wait time should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_PING_RESPONSE_WAIT_TIME.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim()
							.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_PING_RESP_WAIT_TIME);
			errorMessage = "Value of Ping response wait time can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Value of Ping response wait time can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 18 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.7.0)
			 * to get the value of Resource Usage Compute Window
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.7.0) to get the value of  Resource Usage Compute Window");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.7.0) to get the value of ResourceUsage Compute Window");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: Value of Resource Usage Compute Window should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_RESOURCE_USAGE_COMPUTER_WINDOW.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim().equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_15);
			errorMessage = "Value of Resource Usage Compute Window response wait time can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Value of Resource Usage Compute Window can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 19 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.8.0)
			 * to get the average value of CPU Threshold
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.8.0) to get the average value of CPU Threshold");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.8.0) to get the average value of CPU Threshold");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: Average value of CPU Threshold should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_AVG_CPU_THRESHOLD.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim().equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_100);
			errorMessage = "Average value of CPU Threshold can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Average value of CPU Threshold can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 20 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.9.0)
			 * to get the average value of Memory Threshold
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.9.0) to get the average value of Memory Threshold");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.9.0) to get the average value of Memory Threshold");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: Average value of Memory Threshold should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_AVG_MEMORY_THRESHOLD.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim().equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_100);
			errorMessage = "Average value of Memory Threshold can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Average value of Memory Threshold can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 21 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.10.0)
			 * to get the value of maximum Reboot Count
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ " : DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.10.0) to get the value of maximum Reboot Count");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.10.0) to get the value of maximum Reboot Count");
			LOGGER.info("STEP " + stepNum
					+ " : EXPECTED: Value of maximum Reboot Count should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_MAX_REBOOT_COUNT.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim().equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
			errorMessage = "Value of maximum Reboot Count can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Value of maximum Reboot Count can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 22 : Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.11.0)
			 * to get the value of maximum Subsystem Reset Count
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ " : DESCRIPTION: Execute SNMP v3  WALK command on OID (1.3.6.1.4.1.17270.44.1.1.11.0) to get the value of maximum Subsystem Reset Count");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 WALK command on OID (1.3.6.1.4.1.17270.44.1.1.11.0) to get the value of maximum Subsystem Reset Count");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: Value of maximum Subsystem Reset Count should be retrieved using SNMP v3 Query");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_SELFHEAL_MAX_SUB_SYSTEM_RESET_COUNT.getOid());
			status = CommonMethods.isNotNull(response)
					&& !CommonMethods.patternMatcher(response, BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
					&& response.split("=")[1].trim().equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
			errorMessage = "Value of maximum Subsystem Reset Count can't be retrieved using SNMP v3 Query";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Value of maximum Subsystem Reset Count can be retrieved using SNMP v3 Query");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			/**
			 * Step 23 : Execute SNMP v3 SET command on OID (1.3.6.1.2.1.1.1.0) which is a
			 * READ-ONLY MIB
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3 SET command on OID (1.3.6.1.2.1.1.1.0) which is a READ-ONLY MIB");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 SET command on OID (1.3.6.1.2.1.1.1.0) which is a READ-ONLY MIB");
			LOGGER.info("STEP " + stepNum + ": EXPECTED: Set should not be successful as its a read only MIB");
			LOGGER.info("**********************************************************************************");
			// Set any String Value
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			snmpOutput = BroadBandSnmpUtils.retrieveSnmpV3SetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ECM_SYS_DESCR.getOid(), SnmpDataType.STRING,
					BroadBandTestConstants.PROCESS_NAME_SESHAT);
			String failedReason = CommonMethods.patternFinder(snmpOutput,
					BroadBandTestConstants.PATTERN_FINDER_FAILURE_REASON);
			status = CommonMethods.isNotNull(failedReason)
					&& failedReason.equalsIgnoreCase(BroadBandTestConstants.NOT_WRITABLE);
			errorMessage = "Unable to do SNMP operation, SNMP v3 operation on OID (1.3.6.1.2.1.1.1.0) failed";
			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL: The READ-ONLY MIB can not be writable through SNMP v3");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

			/**
			 * Step 24 : Execute SNMP v3 SET command on OID (.1.3.6.1.4.1.17270.44.1.1.4.0)
			 * with incorrect value out of expected range.
			 * 
			 */
			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION: Execute SNMP v3 SET command on OID (.1.3.6.1.4.1.17270.44.1.1.4.0) with incorrect value out of expected range(Expected Range is between 15 and 1440, both inclusive)");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION: Execute SNMP v3 SET command on OID (.1.3.6.1.4.1.17270.44.1.1.4.0) with incorrect value out of expected range(Expected Range is between 15 and 1440, both inclusive)");
			LOGGER.info("STEP " + stepNum
					+ ": EXPECTED: Set should not be successful as the set value is out of expected range");
			LOGGER.info("**********************************************************************************");
			// Expected Range for Ping Interval is between 15 and 1440, both inclusive
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			snmpOutput = BroadBandSnmpUtils.retrieveSnmpV3SetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ECM_SELFHEAL_PING_INTERVAL.getOid() + ".0", SnmpDataType.UNSIGNED_INTEGER,
					BroadBandTestConstants.STRING_VALUE_SIX);
			failedReason = CommonMethods.patternFinder(snmpOutput,
					BroadBandTestConstants.PATTERN_FINDER_FAILURE_REASON);
			status = CommonMethods.isNotNull(failedReason)
					&& failedReason.equalsIgnoreCase(BroadBandTestConstants.WRONG_VALUE);
			errorMessage = "Unable to do SNMP operation, SNMP v3 operation on OID (.1.3.6.1.4.1.17270.44.1.1.4.0) failed";
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: SNMP v3 SET was not successful as the set value is out of expected range");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		} catch (Exception testException) {
			errorMessage = "Exception occurred while trying to validate Enable/Disable Bridge Mode using SNMP v3: "
					+ testException.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		} finally {
			int postCondition = 0;
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V2.toString());

			status = false;
			boolean isSnmpReactivated = false;
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");

			if (!isBridgeModeDisabled) {
				postCondition++;
				errorMessage = "Failed to disable Bridge mode using webpa and snmp";
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postCondition
						+ ": DESCRIPTION : Verify disabling the bridge mode using webpa/SNMP");
				LOGGER.info("POST-CONDITION " + postCondition + ": ACTION : Set values router using webpa");
				LOGGER.info(
						"POST-CONDITION " + postCondition + ": EXPECTED : Bridge mode should be disabled successfully");
				LOGGER.info("#######################################################################################");
				try {
					LOGGER.info("GOING TO DISABLE BRIDGE MODE USING WEBPA.");
					status = BroadBandWiFiUtils.setWebPaParams(device,
							BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS,
							BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER, BroadBandTestConstants.CONSTANT_0);

					if (!status) {
						isSnmpReactivated = true;
						LOGGER.error("FAILED TO DISABLE BRIDGE MODE USING WEBPA, GOING TO TRY USING SNMP");
						snmpOutput = BroadBandSnmpUtils.retrieveSnmpSetOutputWithGivenIndexOnRdkDevices(device, tapEnv,
								BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(), SnmpDataType.INTEGER,
								BroadBandTestConstants.STRING_VALUE_TWO,
								BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex());
						status = CommonMethods.isNotNull(snmpOutput)
								&& snmpOutput.equals(BroadBandTestConstants.STRING_VALUE_TWO);
					}

					if (isSnmpReactivated) {
						LOGGER.info("GOING TO CHECK IF SNMP PROCESS CAME UP, AFTER DISABLING BRIDGE MODE.");
						status = BroadBandSnmpUtils.checkSnmpIsUp(tapEnv, device);
						LOGGER.info("IS SNMP PROCESS UP AFTER DISABLING BRIDGE MODE USING SNMP : " + status);
					} else {
						LOGGER.info("GOING TO CHECK IF WEBPA PROCESS CAME UP, AFTER DISABLING BRIDGE MODE.");
						status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
						LOGGER.info("IS WEBPA PROCESS UP AFTER DISABLING BRIDGE MODE USING WEBPA : " + status);
					}

				} catch (Exception exception) {
					LOGGER.error(
							"FOLLOWING EXCEPTION OCCURED IN POST - CONDITION, WHILE DISABLING BRIDGE MODE (USING WEBPA) : "
									+ exception.getMessage());
				}
				if (status) {
					LOGGER.info("POST-CONDITION " + postCondition + ": ACTUAL: Bridge mode disabled successfully");
				} else {
					LOGGER.info("POST-CONDITION " + postCondition + ": ACTUAL: " + errorMessage);
				}
			}

			if (!isMocaEnabled) {
				postCondition++;
				BroadBandPostConditionUtils.executePostConditionRevertDefaultMocaStatus(device, tapEnv, true,
						postCondition);
			}
			// Adding a static wait time for the WiFi Driver to be up.
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			if (isPublicWifiEnabled) {
				postCondition++;
				BroadBandPostConditionUtils.executePostConditionToDisablePublicWifi(device, tapEnv, postCondition);
			}

		}
	}

	/**
	 * Test to Verify that the device does not respond to invalid community string
	 * STEPS:
	 * <ol>
	 * <li>Step 1 : Execute SNMP get(read) operation for the MIB sysDescr.0
	 * (1.3.6.1.2.1.1.1.0) with invalid communitystring and verify that the device
	 * gives timeout response</li>
	 * <li>Step 2: Execute SNMP set(write) operation for the MIB docsdevresetnow
	 * (.1.3.6.1.2.1.69.1.1.3.0) with integer 1using invalid community string and
	 * verify that the device gives timeout response</li>
	 * </ol>
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Sathurya Ravi
	 * @refactor anandam
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1021", testDecription = "Verify that the device does not respond to invalid community string using sysDescr.0 (1.3.6.1.2.1.1.1.0)")
	public void testSnmpGetOnCableModemForInvalidCommunityString(Dut device) {
		// stores the test case id
		String testCaseId = "TC-RDKB-SNMP-121";
		// stores the step number
		String stepNumber = "s1";
		// stores the test result
		boolean status = false;
		// stores the error message
		String errorMessage = null;
		// stores the command response
		String snmpGetResponse = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1021");
			LOGGER.info(
					"TEST DESCRIPTION: Test to Verify that the device does not respond to invalid community string");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"Step 1. Execute SNMP get(read) operation for the MIB sysDescr.0 (1.3.6.1.2.1.1.1.0) with invalid community string and verify that the device gives timeout response");
			LOGGER.info(
					"Step 2. Execute SNMP set(write) operation for the MIB docsdevresetnow (.1.3.6.1.2.1.69.1.1.3.0) with integer 1 using invalid community string and verify that the device gives timeout response");

			status = false;
			stepNumber = "s1";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Verify whether the device can respond to invalid Community String ");
			LOGGER.info(
					"STEP 1: ACTION: Execute SNMP get operation for the MIB sysDescr.0 (1.3.6.1.2.1.1.1.0) with invalid community string and verify that the device gives timeout response");
			LOGGER.info("STEP 1: EXPECTED: The device should return timeout response");
			LOGGER.info("**********************************************************************************");
			// Execute SNMP get operation for the MIB sysDescr.0
			// (1.3.6.1.2.1.1.1.0)
			snmpGetResponse = BroadBandSnmpUtils.snmpGetOnEcmForInvalidCommunityString(tapEnv, device,
					BroadBandSnmpMib.ECM_SYS_DESCR.getOid(), BroadBandSnmpMib.ECM_SYS_DESCR.getTableIndex());
			LOGGER.info("SNMP command output for sysDescr.0 (1.3.6.1.2.1.1.1.0) : " + snmpGetResponse);
			if (null != snmpGetResponse) {
				// Verify the response from the SNMP get is a Timeout response
				status = (snmpGetResponse.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
						|| (snmpGetResponse.trim()).isEmpty();
				if (!status) {
					errorMessage = "The device is responding for invalid Community String for the SNMP get on MIB sysDescr.0 (1.3.6.1.2.1.1.1.0) . ACTUAL : Response: "
							+ snmpGetResponse;
					LOGGER.error(errorMessage);
				}
			} else {
				status = false;
				errorMessage = "The device has returned some Junk value for the MIB sysDescr.0 (1.3.6.1.2.1.1.1.0)";
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Successfully verified the SNMP get operation using invalid Community String");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

			/**
			 * Step 2 : Execute SNMP set operation for the MIB DevResetNow
			 * (.1.3.6.1.2.1.69.1.1.3.0) with integer 1 and verify that the device gives
			 * timeout response
			 */
			status = false;
			stepNumber = "s2";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION: Verify whether the SNMP set can be performed with invalid community string");
			LOGGER.info(
					"STEP 2: ACTION: Execute SNMP set operation for the MIB docsdevresetnow (.1.3.6.1.2.1.69.1.1.3.0) with integer 1 using invalid community string and verify that the device gives timeout response");
			LOGGER.info("STEP 2: EXPECTED: The device should return timeout response");
			LOGGER.info("**********************************************************************************");
			// Execute SNMP get operation for the MIB DevResetNow
			// (.1.3.6.1.2.1.69.1.1.3.0)
			snmpGetResponse = BroadBandSnmpUtils.snmpSetOnEcmForInvalidCommunityString(tapEnv, device,
					BroadBandSnmpMib.ECM_RESET_MIB.getOid(), SnmpDataType.INTEGER, "1", "0");
			LOGGER.info("SNMP command output for docsdevresetnow (.1.3.6.1.2.1.69.1.1.3.0) : " + snmpGetResponse);
			if (null != snmpGetResponse) {
				// Verify the response from the SNMP set is a Timeout response
				status = (snmpGetResponse.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
						|| (snmpGetResponse.trim()).isEmpty();
				if (!status) {
					errorMessage = "The device is responding for the invalid Community String for the SNMP get on MIB docsdevresetnow (.1.3.6.1.2.1.69.1.1.3.0) . ACTUAL : Response:  "
							+ snmpGetResponse;
					LOGGER.error(errorMessage);
				}
			} else {
				errorMessage = "The device has returned inappropriate value for the  MIB docsdevresetnow (.1.3.6.1.2.1.69.1.1.3.0). Actual response: "
						+ snmpGetResponse;
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Successfully verified the SNMP set operation using invalid Community String");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, false);

		} catch (Exception exception) {
			errorMessage = "Unable to validate the SNMP get and set with Invalid Community string"
					+ exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1021");
	}

	/**
	 * Test to verify whether the device MTA can respond for invalid community
	 * string
	 * 
	 * <ol>
	 * <li>STEP 1: Perform SNMP get on the MTA IP with Invalid MTA Community
	 * String</li>
	 * <li>STEP 2: Perform SNMP set on the MTA IP with Invalid MTA Community
	 * String</li>
	 * </ol>
	 * 
	 * @author Sathurya Ravi
	 * @param device The device to be used.
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-SNMP-1022", testDecription = "Validate that the device does not give valid SNMP response  for invalid MTA community String")

	public void testSnmpGetSetOnMtaSideForInvalidCommunityString(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-122";
		String stepNumber = "s1";
		boolean status = false;
		String errorMessage = null;
		String snmpGetResponse = null;
		String mtaAddress = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_COMMAND_MTA_IP_OF_DEVICE);
		LOGGER.info("mtaAddress:  " + mtaAddress);

		try {

			if (CommonMethods.isNotNull(mtaAddress) && !mtaAddress.equals("0.0.0.0")) {
				status = false;
				stepNumber = "s1";

				/**
				 * Step 1 : Perform SNMP get on the MTA IP with Invalid MTA Community String
				 */

				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 1: DESCRIPTION: Perform SNMP get on the MTA IP with Invalid MTA Community String ");
				LOGGER.info(
						"STEP 1: ACTION: Execute SNMP get on the MIB(sysDescr.0) for the MTA IP with Invalid MTA Community string.");
				LOGGER.info("STEP 1: EXPECTED: The device should return timeout response");
				LOGGER.info("**********************************************************************************");

				// Execute SNMP get operation for the MIB sysDescr.0 (1.3.6.1.2.1.1.1.0)
				snmpGetResponse = BroadBandSnmpUtils.snmpGetOnEmtaForInvalidCommunityString(tapEnv, device,
						BroadBandSnmpMib.ECM_SYS_DESCR.getOid(), BroadBandSnmpMib.ECM_SYS_DESCR.getTableIndex(),
						mtaAddress);
				LOGGER.info("SNMP command output for sysDescr.0 (1.3.6.1.2.1.1.1.0) : " + snmpGetResponse);

				if (null != snmpGetResponse) {
					// Verify the response from the SNMP get is a Timeout response
					status = (snmpGetResponse.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
							|| (snmpGetResponse.trim()).isEmpty();
					if (!status) {
						errorMessage = "Seems like the device is responding for invalid Community String on the SNMP get on MIB sysDescr.0 (1.3.6.1.2.1.1.1.0) . ACTUAL : Response: "
								+ snmpGetResponse + errorMessage;
						LOGGER.error(errorMessage);
					}
				} else {
					status = false;
					errorMessage = "The device has returned some Junk value for the MIB sysDescr.0 (1.3.6.1.2.1.1.1.0)";
					LOGGER.error(errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage,
						false);

				status = false;
				stepNumber = "s2";

				/**
				 * Step 2 : Execute SNMP set operation for the MIB(1.3.6.1.2.1.2.2.1.7.9.0) with
				 * integer 2 and verify that the device gives timeout response
				 */

				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 2: DESCRIPTION: Perform SNMP set on the MTA IP with Invalid MTA Community String");
				LOGGER.info(
						"STEP 2: ACTION: Execute SNMP set on the MIB(1.3.6.1.2.1.2.2.1.7.9.0) with interger 2 and Invalid MTA community string. ");
				LOGGER.info("STEP 2: EXPECTED: The device should return timeout response");
				LOGGER.info("**********************************************************************************");

				// Execute SNMP get operation for the MIB MIB(1.3.6.1.2.1.2.2.1.7.9.0)

				snmpGetResponse = BroadBandSnmpUtils.snmpSetOnEmtaForInvalidCommunityString(tapEnv, device,
						BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_NEGATIVE_SCENARIO.getOid(), SnmpDataType.INTEGER,
						"2", BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_NEGATIVE_SCENARIO.getTableIndex(), mtaAddress);
				LOGGER.info("SNMP command output for MIB(1.3.6.1.2.1.2.2.1.7.9.0) : " + snmpGetResponse);

				if (null != snmpGetResponse) {
					// Verify the response from the SNMP set is a Timeout response
					status = (snmpGetResponse.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
							|| (snmpGetResponse.trim()).isEmpty();
					if (!status) {
						errorMessage = "Seems like the device is responding for the invalid Community String on the SNMP set on MIB(1.3.6.1.2.1.2.2.1.7.9.0) . ACTUAL : Response:  "
								+ snmpGetResponse + errorMessage;
						LOGGER.error(errorMessage);
					}
				} else {
					status = false;
					errorMessage = "The device has returned some Junk value for the  MIB(1.3.6.1.2.1.2.2.1.7.9.0)";
					LOGGER.error(errorMessage);
				}

				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage,
						false);
			} else {
				errorMessage = "Skipping the tests as this device is not MTA provisioned.Marking the tests as not applicable ";
				LOGGER.error(errorMessage);
				for (int i = 1; i <= 2; i++) {
					tapEnv.updateExecutionForAllStatus(device, testCaseId, "s".concat(String.valueOf(i)),
							ExecutionStatus.NOT_APPLICABLE, errorMessage, false);
				}
			}

		} catch (Exception exception) {
			status = false;
			errorMessage = "Unable to validate the SNMP get and set with Invalid Community string for MTA"
					+ exception.getMessage();
			LOGGER.error(errorMessage);
		}

	}

	/**
	 * Verify setting the AAA server Primary & Secondary IP addresses (IPv4) for 2.4
	 * 5GHz
	 * <ol>
	 * 
	 * <li>Verify setting the AAA server Primary IP address (IPv4 ) for 2.4GHz.</li>
	 * <li>Verify setting the AAA server Secondary IP address (IPv4 ) for
	 * 2.4GHz.</li>
	 * <li>Verify setting the AAA server Primary IP address (IPv4 ) for 5GHz.</li>
	 * <li>Verify setting the AAA server Secondary IP address (IPv4 ) for 5GHz.</li>
	 * <li>POST-CONDITION 1: Verify all the AAA server Primary and Secondary
	 * Addresses for 2.4GHz are set back to default.</li>
	 * <li>POST-CONDITION 2: Verify all the AAA server Primary and Secondary
	 * Addresses for 5GHz are set back to default.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author prashant.mishra
	 * @refactor anandam
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-SNMP-1010", testDecription = "Verify setting the AAA server Primary & Secondary IP addresses (IPv4) for 2.4 & 5GHz")
	public void testToverifyConfiguringAaaAddressesforBothSsids(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-SNMP-110";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1010");
		LOGGER.info(
				"TEST DESCRIPTION: Verify setting the AAA server Primary & Secondary IP addresses (IPv4) for 2.4 & 5GHz");

		LOGGER.info("TEST STEPS : ");

		LOGGER.info("1. Verify setting the AAA server Primary IP address (IPv4 ) for 2.4GHz.");
		LOGGER.info("2. Verify setting the AAA server Secondary IP address (IPv4 ) for 2.4GHz.");
		LOGGER.info("3. Verify setting the AAA server Primary IP address (IPv4 ) for 5GHz.");
		LOGGER.info("4. Verify setting the AAA server Secondary IP address (IPv4 ) for 5GHz.");
		LOGGER.info(
				"POST-CONDITION 1: Verify all the AAA server Primary and Secondary Addresses for 2.4GHz are set back to default.");
		LOGGER.info(
				"POST-CONDITION 2: Verify all the AAA server Primary and Secondary Addresses for 5GHz are set back to default.");
		LOGGER.info("#######################################################################################");
		try {

			stepNum = "S1";
			errorMessage = "Unable to configure the AAA server Primary IP address (IPv4 ) for 2.4GHz.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify setting the AAA server Primary IP address (IPv4 ) for 2.4GHz.");
			LOGGER.info("STEP 1: ACTION : Execute the webpa Set command with "
					+ BroadBandWebPaConstants.WEBPA_PARAM_2_4GHZ_WIFI_10005_RADIUSSERVERIPADDR + " and set the value.");
			LOGGER.info("STEP 1: EXPECTED : AAA server Primary IP address (IPv4 )should be set.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4GHZ_WIFI_10005_RADIUSSERVERIPADDR,
					BroadBandTestConstants.CONSTANT_0, AutomaticsPropertyUtility
					.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_RADIUS_SERVER_IPADDR));
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : AAA server Primary IP address (IPv4 ) for 2.4GHz is configured successfully.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S2";
			errorMessage = "Unable to configure the AAA server Secondary IP address (IPv4 ) for 2.4GHz.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify setting the AAA server Secondary IP address (IPv4 ) for 2.4GHz.");
			LOGGER.info("STEP 2: ACTION : Execute the webpa Set command with "
					+ BroadBandWebPaConstants.WEBPA_PARAM_2_4GHZ_WIFI_10005_SECONDARY_RADIUSSERVERIPADDR
					+ " and set the value.");
			LOGGER.info("STEP 2: EXPECTED : AAA server Secondary IP address (IPv4 )should be set.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4GHZ_WIFI_10005_SECONDARY_RADIUSSERVERIPADDR,
					BroadBandTestConstants.CONSTANT_0, AutomaticsPropertyUtility
					.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_RADIUS_SERVER_IPADDR));
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : AAA server Secondary IP address (IPv4 ) for 2.4GHz is configured successfully.");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S3";
			errorMessage = "Unable to configure the AAA server Primary IP address (IPv4 ) for 5GHz.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify setting the AAA server Primary IP address (IPv4 ) for 5GHz.");
			LOGGER.info("STEP 3: ACTION : Execute the webpa Set command with "
					+ BroadBandWebPaConstants.WEBPA_PARAM_5GHZ_WIFI_10105_RADIUSSERVERIPADDR + " and set the value.");
			LOGGER.info("STEP 3: EXPECTED : AAA server Primary IP address (IPv4 )should be set.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_5GHZ_WIFI_10105_RADIUSSERVERIPADDR,
					BroadBandTestConstants.CONSTANT_0, AutomaticsPropertyUtility
					.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_RADIUS_SERVER_IPADDR));
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : AAA server Primary IP address (IPv4 ) for 5GHz is configured successfully.");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S4";
			errorMessage = "Unable to configure the AAA server Secondary IP address (IPv4 ) for 5GHz.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify setting the AAA server Secondary IP address (IPv4 ) for 5GHz.");
			LOGGER.info("STEP 4: ACTION : Execute the webpa Set command with "
					+ BroadBandWebPaConstants.WEBPA_PARAM_5GHZ_WIFI_10105_SECONDARY_RADIUSSERVERIPADDR
					+ " and set the value");
			LOGGER.info("STEP 4: EXPECTED : AAA server Secondary IP address (IPv4 )should be set");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_5GHZ_WIFI_10105_SECONDARY_RADIUSSERVERIPADDR,
					BroadBandTestConstants.CONSTANT_0, AutomaticsPropertyUtility
					.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_RADIUS_SERVER_IPADDR));

			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : AAA server Secondary IP address (IPv4 ) for 5GHz is configured successfully.");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");

			boolean executionStatusprimary = false;
			boolean executionStatusSecondary = false;
			status = false;
			errorMessage = "Unable to revert the value of AAA server Primary and Secondary Addresses for 2.4GHz to default.";
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 1 : DESCRIPTION : Verify all the AAA server Primary and Secondary Addresses for 2.4GHz are set back to default.");
			LOGGER.info(
					"POST-CONDITION 1 : ACTION : Execute the SNMP Set command with OID: .1.3.6.1.4.1.17270.50.2.2.3.2.1.2.10005 & .1.3.6.1.4.1.17270.50.2.2.3.2.1.6.10005 and set the value to '0.0.0.0'");
			LOGGER.info(
					"POST-CONDITION 1 : EXPECTED : AAA server Primary and Secondary Addresses for 2.4GHz should be set back to default value.");
			LOGGER.info("#######################################################################################");
			executionStatusprimary = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4GHZ_WIFI_10005_RADIUSSERVERIPADDR,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_NULL_IP);
			executionStatusSecondary = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4GHZ_WIFI_10005_SECONDARY_RADIUSSERVERIPADDR,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_NULL_IP);
			status = executionStatusprimary && executionStatusSecondary;
			errorMessage = " Failed to revert 2.4 GHz Radius server IP  to null";

			if (status) {
				LOGGER.info(
						"POST-CONDITION 1 : ACTUAL : AAA server Primary and Secondary Addresses for 2.4GHz are reverted to default successfully.");
			} else {
				LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
			}
			status = false;
			errorMessage = "Unable to revert the value of AAA server Primary and Secondary Addresses for 5GHz to default.";
			executionStatusprimary = false;
			executionStatusSecondary = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 2 : DESCRIPTION : Verify all the AAA server Primary and Secondary Addresses for 5GHz are set back to default.");
			LOGGER.info(
					"POST-CONDITION 2 : ACTION : Execute the SNMP Set command with OID: .1.3.6.1.4.1.17270.50.2.2.3.2.1.2.10105 & .1.3.6.1.4.1.17270.50.2.2.3.2.1.6.10105 and set the value to \"0.0.0.0\"");
			LOGGER.info(
					"POST-CONDITION 2 : EXPECTED : AAA server Primary and Secondary Addresses for 5GHz should be set back to default value.");
			LOGGER.info("#######################################################################################");
			executionStatusprimary = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_5GHZ_WIFI_10105_RADIUSSERVERIPADDR,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_NULL_IP);
			executionStatusSecondary = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_5GHZ_WIFI_10105_SECONDARY_RADIUSSERVERIPADDR,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_NULL_IP);

			status = executionStatusprimary && executionStatusSecondary;
			errorMessage = " Failed to revert 5 GHz Radius server IP  to null";
			if (status) {
				LOGGER.info(
						"POST-CONDITION 2 : ACTUAL : AAA server Primary and Secondary Addresses for 5GHz are reverted to default successfully.");
			} else {
				LOGGER.error("POST-CONDITION 2 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1010");

	}

	/**
	 * Method to Upgrade device via HTTP Server Protocol
	 * 
	 * Verify Device Upgrade via SNMP IPv6 HTTP protocol.
	 * <ol>
	 * <li>Verify getting the latest image file name.</li>
	 * <li>Verify SNMP MIB docsDevSwServerTransportProtocol is set to HTTP.</li>
	 * <li>Verify SNMP MIB docsDevSwServerAddressType is set to IPv6 address
	 * type.</li>
	 * <li>Verify SNMP MIB docsDevSwServerAddress is set to HTTP CDL server address
	 * in Hex format</li>
	 * <li>Verify SNMP MIB docsDevSwFilename is set to latest firmware
	 * filename.</li>
	 * <li>Verify SNMP MIB docsDevSwAdminStatus is set for starting the
	 * download.</li>
	 * <li>Verify SNMP code download is in progress.</li>
	 * <li>Verify SNMP code download completed successfully.</li>
	 * <li>Verify CDL has happened successfully and new image is upgraded.</li>
	 * <li>POST CONDITION 1: Verify device firmaware is revert back.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Prashant Mishra
	 * @Refactor Athira
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-CDL-5003")
	public void testToverifyDeviceUpgradeOrDowngradeViaHttpPrtocol(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-CDL-503";
		String stepNum = "";
		String errorMessage = "";
		String currentImageName = "";
		String latestImageNameToUpgrade = "";
		String snmpSetResponse = "";
		String snmpGetResonse = "";
		String snmpSetAdminStatus = "";
		boolean status = false;
		boolean hasLatestBuildChanged = false;
		// Variable Declaration ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-5003");
		LOGGER.info("TEST DESCRIPTION: Verify Device Upgrade via SNMP IPv6 HTTP protocol.");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify getting the latest image file name.");
		LOGGER.info("2. Verify SNMP MIB docsDevSwServerTransportProtocol is set to HTTP.");
		LOGGER.info("3. Verify SNMP MIB docsDevSwServerAddressType is set to IPv6 address type.");
		LOGGER.info("4. Verify SNMP MIB docsDevSwServerAddress is set to HTTP CDL server address in Hex format");
		LOGGER.info("5. Verify SNMP MIB docsDevSwFilename is set to latest firmware filename.");
		LOGGER.info("6. Verify SNMP MIB docsDevSwAdminStatus is set for starting the download.");
		LOGGER.info("7. Verify SNMP code download is in progress.");
		LOGGER.info("8. Verify SNMP code download completed successfully.");
		LOGGER.info("9. Verify CDL has happened successfully and new image is upgraded.");
		LOGGER.info("POST CONDITION 1. Verify device firmaware is revert back.");

		LOGGER.info("#######################################################################################");
		try {
			stepNum = "S1";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify getting the latest and current image name.");
			LOGGER.info("STEP 1: ACTION : Get latest and current image name.");
			LOGGER.info("STEP 1: EXPECTED : Both latest and current Image name should not be null.");
			LOGGER.info("**********************************************************************************");
			currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT IMAGE OF THE DEVICE: " + currentImageName);

			LOGGER.info("LATEST FIRMWARE VERSION: " + latestImageNameToUpgrade);
			if (CommonMethods.isNull(latestImageNameToUpgrade)) {
				LOGGER.info(
						" GA image obtained from deployed version service is null. Hence getting the image from property file ");
				latestImageNameToUpgrade = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
						device, BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
				LOGGER.info("Latest Firmware version from property file: " + latestImageNameToUpgrade);
			}

			LOGGER.info("LATEST IMAGE TO UPGRADE: " + latestImageNameToUpgrade);
			status = CommonMethods.isNotNull(currentImageName) && CommonMethods.isNotNull(latestImageNameToUpgrade)
					&& !latestImageNameToUpgrade.equals(currentImageName);
			errorMessage = latestImageNameToUpgrade.equals(currentImageName)
					? "Device is already having latest Image file."
					: "Unable to get latest and current image name.";
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Latest and current Image name obtained successfully.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			stepNum = "S2";
			errorMessage = "snmpset on docsDevSwServerTransportProtocol(1.3.6.1.2.1.69.1.3.8.0) failed.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify SNMP MIB docsDevSwServerTransportProtocol is set to HTTP.");
			LOGGER.info(
					"STEP 2: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.8.0 and value to be set as  2.");
			LOGGER.info("STEP 2: EXPECTED : SNMP MIB  docsDevSwServerTransportProtocol should be set to HTTP(2).");
			LOGGER.info("**********************************************************************************");
			snmpSetResponse = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_TWO);
			LOGGER.info("STEP 2: BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL.getOid() is "
					+ BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL.getOid()
					+ "BroadBandTestConstants.STRING_VALUE_TWO is " + BroadBandTestConstants.STRING_VALUE_TWO);
			LOGGER.info("SNMPSET RESPONSE FOR docsDevSwServerTransportProtocol: " + snmpSetResponse);
			if (snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO)) {
				snmpGetResonse = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
						BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL.getOid());
				LOGGER.info("Step 2: BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL.getOid() is"
						+ BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL.getOid());
				LOGGER.info("SNMPGET RESPONSE FOR docsDevSwServerTransportProtocol: " + snmpGetResonse);
				status = snmpGetResonse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO);
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : SNMP MIB  docsDevSwServerTransportProtocol is set to HTTP(2).");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S3";
			errorMessage = "snmpset on docsDevSwServerAddressType(1.3.6.1.2.1.69.1.3.6.0) failed.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Verify SNMP MIB docsDevSwServerAddressType is set to IPv6 address type.");
			LOGGER.info(
					"STEP 3: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.6.0 and value to be set as  2.");
			LOGGER.info(
					"STEP 3: EXPECTED : SNMP MIB  docsDevSwServerAddressType should be set to IPv6 address type(2).");
			LOGGER.info("**********************************************************************************");
			snmpSetResponse = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_TWO);
			LOGGER.info("STEP 3: BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL.getOid() is "
					+ BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL.getOid()
					+ "BroadBandTestConstants.STRING_VALUE_TWO is " + BroadBandTestConstants.STRING_VALUE_TWO);
			LOGGER.info("SNMP RESPONSE FOR docsDevSwServerAddressType: " + snmpSetResponse);
			if (snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO)) {
				LOGGER.info("STEP 3: BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL.getOid() is "
						+ BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL.getOid());
				snmpGetResonse = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
						BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL.getOid());
				LOGGER.info("SNMP RESPONSE FOR docsDevSwServerAddressType: " + snmpGetResonse);
				status = snmpGetResonse.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO);
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : SNMP MIB  docsDevSwServerAddressType is set to IPv6 address type(2).");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			stepNum = "S4";
			errorMessage = "snmpset on docsDevSwServerAddress(1.3.6.1.2.1.69.1.3.7.0) with server address protocol failed.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify SNMP MIB docsDevSwServerAddress is set to HTTP CDL server address in Hex format");
			LOGGER.info(
					"STEP 4: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.7.0 and value to be set as  server address.");
			LOGGER.info(
					"STEP 4: EXPECTED : SNMP MIB  docsDevSwServerAddressType should be set to HTTP CDL server address in Hex format.");
			LOGGER.info("**********************************************************************************");
			status = FirmwareDownloadUtils.setServerAddressForSnmpCodeDownload(tapEnv, device);
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : SNMP MIB  docsDevSwServerAddressType is set to HTTP CDL server address in Hex format.");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S5";
			errorMessage = "snmpset on docsDevSwFilename(1.3.6.1.2.1.69.1.3.2.0) with latest image version failed.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify SNMP MIB docsDevSwFilename is set to latest firmware filename.");
			LOGGER.info(
					"STEP 5: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.2.0 and value to be set as Image file name obtained in step 1.");
			LOGGER.info("STEP 5: EXPECTED : SNMP MIB  docsDevSwFilename should be set to latest firmware.");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("Step 5: BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid() is"
					+ BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid()
					+ "latestImageNameToUpgrade + BroadBandCdlConstants.BIN_EXTENSION" + latestImageNameToUpgrade
					+ BroadBandCdlConstants.BIN_EXTENSION);
			snmpSetResponse = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid(), SnmpDataType.STRING,
					latestImageNameToUpgrade + BroadBandCdlConstants.BIN_EXTENSION);
			LOGGER.info("SNMPSET RESPONSE FOR docsDevSwFilename: " + snmpSetResponse);
			if (CommonMethods.patternMatcher(snmpSetResponse.toLowerCase(), latestImageNameToUpgrade.toLowerCase())) {
				LOGGER.info("STEP 5: BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid() is "
						+ BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid());
				snmpGetResonse = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
						BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid());
				LOGGER.info("SNMPGET RESPONSE FOR docsDevSwFilename: " + snmpGetResonse);
				status = CommonMethods.patternMatcher(snmpGetResonse.toLowerCase(),
						latestImageNameToUpgrade.toLowerCase());
			}
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : SNMP MIB  docsDevSwFilename is set to latest firmware.");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			stepNum = "S6";
			errorMessage = "snmpset on docsDevSwAdminStatus(1.3.6.1.2.1.69.1.3.3.0) as 1 failed.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify SNMP MIB docsDevSwAdminStatus is set for starting the download.");
			LOGGER.info(
					"STEP 6: ACTION : Execute SNMP SET command with  oid as 1.3.6.1.2.1.69.1.3.3.0 and value to be set as  1.");
			LOGGER.info("STEP 6: EXPECTED : SNMP MIB  docsDevSwAdminStatus should be set to 1.");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS.getOid() is "
					+ BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS.getOid());
			snmpSetAdminStatus = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_CONSTANT_1);
			LOGGER.info("SNMPSET RESPONSE FOR snmpSetAdminStatus: " + snmpSetAdminStatus);
			if (snmpSetAdminStatus.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_1)) {
				LOGGER.info("STEP 6: BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS.getOid() is "
						+ BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS.getOid());
				snmpGetResonse = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
						BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS.getOid());
				LOGGER.info("SNMPGET RESPONSE FOR snmpGetResonse: " + snmpGetResonse);
				status = snmpGetResonse.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_1);
			}
			if (status) {
				LOGGER.info("STEP 6: ACTUAL : SNMP MIB  docsDevSwAdminStatus is set to 1 successfully.");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			stepNum = "S7";
			errorMessage = "SNMP code download has not started.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify SNMP code download is in progress.");
			LOGGER.info("STEP 7: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.4.0.");
			LOGGER.info("STEP 7: EXPECTED : SNMP should return the Integer value 1.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCodeDownloadUtils.isUgradeStatusInProgressUsingSnmpCommand(tapEnv, device);
			if (status) {
				LOGGER.info("STEP 7: ACTUAL : SNMP code download via HTTP server started successfully.");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			stepNum = "S8";
			errorMessage = "SNMP code download has not completed successfully.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify SNMP code download completed successfully.");
			LOGGER.info("STEP 8: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.4.0.");
			LOGGER.info("STEP 8: EXPECTED : SNMP should return the Integer value 3.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCodeDownloadUtils.verifySnmpCodeDownlaodCompletionStatus(tapEnv, device);
			hasLatestBuildChanged = status;
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : SNMP code download completed successfully.");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S9";
			errorMessage = "Image file name obtained in step 1 and image file name of device should be same.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify CDL has happened successfully and new image is upgraded.");
			LOGGER.info("STEP 9: ACTION : Execute SNMP GET command with  oid as 1.3.6.1.2.1.69.1.3.5");
			LOGGER.info("STEP 9: EXPECTED : SNMP should return the image file name of the device.");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("Waiting for 1 minute to verify upgraded image name after successful download.");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = BroadBandCodeDownloadUtils.isImageUpgradedInDevice(tapEnv, device, latestImageNameToUpgrade);
			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Image upgraded successfully in device.");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			if (hasLatestBuildChanged) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				LOGGER.info("POST-CONDITION STEPS");
				LOGGER.info("ImageName to revert :" + currentImageName);
				if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)) {
					BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged,
							BroadBandTestConstants.BOOLEAN_VALUE_FALSE, BroadBandTestConstants.CONSTANT_0,
							currentImageName);
				}
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
		}
	}

	/**
	 * Test to verify resetting the device through SNMP v3 is successful
	 * 
	 * <ol>
	 * <li>PRE-CONDITION 1 : Disable codebig first enable using webpa</li>
	 * <li>STEP 1 : Enable SNMPv3 support using RFC</li>
	 * <li>STEP 2 : Verify SNMPv3 support parameter is enabled in dcmrfc.log
	 * file</li>
	 * <li>STEP 3 : Verify SNMPv3 support is enabled using webpa</li>
	 * <li>STEP 4: Execute SNMP v3 SET command on
	 * OID(1.3.6.1.4.1.17270.50.2.1.1.1.0) with set value as 4 to Reset the
	 * device</li>
	 * <li>STEP 5: Verify the device comes up after the reset operation is
	 * complete</li>
	 * <li>STEP 6 : Backup SecConsole log and PAMlog to nvram</li>
	 * <li>STEP 7: Execute SNMP v3 SET command on
	 * OID(1.3.6.1.4.1.17270.50.2.1.1.1002.0) with set value as 2 to Factory reset
	 * the device</li>
	 * <li>STEP 8: Verify the device comes up after the Factory reset operation is
	 * complete</li>
	 * <li>STEP 9 : Validate for after reboot Device led logs in SecConsole.txt.0
	 * and PAMlog.txt.0</li>
	 * <li>STEP 10 : Validate before reboot Device led logs in Backup
	 * SecConsole.txt.0 and PAMlog.txt.0 In nvram</li>
	 * </ol>
	 * 
	 * @author Sathya Kishore
	 * @refactor Athira
	 * 
	 * @param device The device to be used.
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1014")
	public void snmpV3TestToResetTheDevice(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-114";
		String stepNumber = "s1";
		boolean status = false; // stores the test status
		String errorMessage = null; // stores the error message
		String response = null;
		int stepNum = 1;
		boolean isFactoryReset = false;
		Map<String, String> backupMap = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE : TC-RDKB-SNMP-1014");
			LOGGER.info(
					"TEST DESCRIPTION: Test to Verify resetting and Factory resetting the device through SNMP v3 is successful");
			LOGGER.info("PRE-CONDITION 1 : Disable codebig first enable using webpa");
			LOGGER.info("STEP 1 : Enable SNMPv3 support using RFC");
			LOGGER.info("STEP 2 : Verify SNMPv3 support parameter is enabled in dcmrfc.log file");
			LOGGER.info("STEP 3 : Verify SNMPv3 support is enabled using webpa");
			LOGGER.info(
					"STEP 4: Execute SNMP v3 SET command on OID(1.3.6.1.4.1.17270.50.2.1.1.1.0) with set value as 4 to Reset the device");
			LOGGER.info("STEP 5: Verify the device comes up after the reset operation is complete");
			LOGGER.info("STEP 6 : Backup SecConsole log and PAMlog to nvram");
			LOGGER.info(
					"STEP 7: Execute SNMP v3 SET command on OID(1.3.6.1.4.1.17270.50.2.1.1.1002.0) with set value as 2 to Factory reset the device");
			LOGGER.info("STEP 8: Verify the device comes up after the Factory reset operation is complete");
			LOGGER.info("STEP 9 : Validate for after reboot Device led logs in SecConsole.txt.0 and PAMlog.txt.0");
			LOGGER.info(
					"STEP 10 : Validate before reboot Device led logs in Backup SecConsole.txt.0 and PAMlog.txt.0 In nvram");
			LOGGER.info("POST-CONDITION 1: Reactivate the Device");
			LOGGER.info("POST-CONDITION 2: Delete Temporary Files in given path");
			LOGGER.info("POST-CONDITION 3: VERIFY THE PUBLICWIFI STATUS IS ENABLED");
			LOGGER.info("#######################################################################################");

			String snmpOutput = null; // stores SNMP output
			boolean isSNMPv3Enabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_SNMPV3_SUPPORT, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			LOGGER.info("################################# STARTING PRE-CONFIGURATIONS #############################");
			BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

			if (!isSNMPv3Enabled) {
				stepNumber = "S" + stepNum;
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Enable SNMPv3 support using RFC");
				LOGGER.info("STEP " + stepNum + ": ACTION: 1. update RFC server url 2. post payload data 3. reboot");
				LOGGER.info("STEP " + stepNum + ": EXPECTED: Device rebooted after posting rfc profile successfully");
				LOGGER.info("******************************************************************************");
				errorMessage = "Failed to enable SNMPv3 support using RFC";
				status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
						BroadBandTestConstants.SNMPV3, true);
				status = true;
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: Successfully posted the payload data to enable SNMPv3 support & rebooted the device to get the latest RFC configuration ");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

				stepNum++;
				stepNumber = "S" + stepNum;
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNum
						+ ": DESCRIPTION: Verify SNMPv3 support parameter is enabled in dcmrfc.log file");
				LOGGER.info("STEP " + stepNum
						+ ": ACTION: Execute command: grep -i \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support value=true\" /rdklogs/logs/dcmrfc.log");
				LOGGER.info("STEP " + stepNum
						+ ": EXPECTED: log message of SNMPv3 support value = true in dcmrfc.log file should present");
				LOGGER.info("******************************************************************************");
				errorMessage = "Failed to get log message of SNMPv3 support value = true in dcmrfc.log file";

				try {
					response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_SNMPV3_SUPPORT,
							BroadBandCommandConstants.FILE_DCMRFC_LOG, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

					status = CommonMethods.isNotNull(response)
							&& CommonUtils.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.TRUE);
				} catch (Exception e) {
					LOGGER.error(
							"Exception occured while verifying log message of SNMPv3 support value = true in dcmrfc.log file"
									+ e.getMessage());
				}
				if (status) {
					LOGGER.info("STEP " + stepNum + ": ACTUAL: Successfully enabled the SNMPv3 support using RFC");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

				stepNum++;
				stepNumber = "S" + stepNum;
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Verify SNMPv3 support is enabled using webpa ");
				LOGGER.info("STEP " + stepNum
						+ ": ACTION: Execute webpa command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support");
				LOGGER.info("STEP " + stepNum + ": EXPECTED: Webpa get request is success and parameter value is true");
				LOGGER.info("******************************************************************************");
				errorMessage = "Failed to get the response for webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support";

				try {
					((Device) device).setErouterIpAddress(CommonUtils.getDeviceIpAddressFromBhc(device,
							RDKBTestConstants.DEVICE_IP_ADDRESS_TYPE_ESTB));
					response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_SNMPV3_SUPPORT);
					status = CommonMethods.isNotNull(response)
							&& response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
				} catch (Exception e) {
					LOGGER.error("Exception occured while verifying SNMPv3 value using webpa" + e.getMessage());
				}
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: Successfully verified the SNMPv3 support is enabled using webpa operation");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				errorMessage = "Snmpv3 is by default enabled. So step 1-3 is not applicable as SNMPv3 is already enabled in the device ";
				int stepNo = 1;
				while (stepNo <= 3) {
					stepNumber = "s" + stepNo;
					errorMessage += ",Marked as Not Applicable";
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNo++;
				}

			}

			/**
			 * Step 4 : Execute SNMP v3 SET command on OID(1.3.6.1.4.1.17270.50.2.1.1.1.0)
			 * with set value as 4 to Reset the device
			 * 
			 */

			stepNum = BroadBandTestConstants.CONSTANT_4;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ "DESCRIPTION: Execute SNMP v3 SET command on OID(1.3.6.1.4.1.17270.50.2.1.1.1.0) with set value as 4 to Reset the device");
			LOGGER.info("STEP " + stepNum
					+ "ACTION: Execute SNMP v3 SET command on OID(1.3.6.1.4.1.17270.50.2.1.1.1.0) with set value as 4 to Reset the device");
			LOGGER.info("STEP " + stepNum
					+ "EXPECTED: On executing this command, SNMP should return the Integer value 4 and Reset should be successful and devivce should come up");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			snmpOutput = BroadBandSnmpUtils.retrieveSnmpV3SetOutputWithDefaultIndexOnRdkDevices_V3(device, tapEnv,
					BroadBandSnmpMib.ESTB_REBOOT_DEVICE.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_4);
			status = CommonMethods.isNotNull(snmpOutput) && snmpOutput.equals(BroadBandTestConstants.STRING_VALUE_4);
			errorMessage = "Unable to reset the device using the SNMP v3 Command";
			if (status) {
				status = BroadBandCommonUtils.verifySTBRebootAndStbAccessible(device, tapEnv);
				errorMessage = "Device reset does not happen after successful SNMP v3 Command Execution";
			}
			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL: Resetting the device using SNMP v3 command is successful");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/** Step 5 : Backup Secconsole,Pamlog logs to given path */
			backupMap = helperMethodToBackupFiles(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_5);

			/**
			 * Step 6 : Execute SNMP v3 SET command on
			 * OID(1.3.6.1.4.1.17270.50.2.1.1.1002.0) with set value as 2 to Factory reset
			 * the device
			 * 
			 */
			stepNum = BroadBandTestConstants.CONSTANT_6;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ "DESCRIPTION: Execute SNMP v3  SET command on OID(1.3.6.1.4.1.17270.50.2.1.1.1002.0) with set value as 2 to Factory reset the device");
			LOGGER.info("STEP " + stepNum
					+ "ACTION: Execute SNMP v3  SET command on OID(1.3.6.1.4.1.17270.50.2.1.1.1002.0) with set value as 2 to Factory reset the device");
			LOGGER.info("STEP " + stepNum
					+ "EXPECTED: On executing this command, SNMP should return the Integer value 2 and Factory Reset should be successful and device should come up");
			LOGGER.info("**********************************************************************************");
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());

			snmpOutput = BroadBandSnmpUtils.retrieveSnmpV3SetOutputWithDefaultIndexOnRdkDevices(device, tapEnv,
					BroadBandSnmpMib.ESTB_FACTORY_RESET_DEVICE.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_ONE);
			LOGGER.info("snmpOutput after FR the device " + snmpOutput);

			status = CommonMethods.isNotNull(snmpOutput) && snmpOutput.equals(BroadBandTestConstants.STRING_VALUE_ONE);
			errorMessage = "Unable to factory reset the device using the SNMP v3 Command";
			if (status) {
				// After successful Factory reset, device should not be accessible, to ensure
				// device started rebooting

				status = BroadBandCommonUtils.verifySTBRebootAndStbAccessible(device, tapEnv);

				errorMessage = "Device Factory reset does not happen after successful SNMP v3 Command Execution";

			}
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Factory Resetting the device using SNMP v3 command is successful");
				isFactoryReset = status;
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/** Step 7 and Step 8 To validate LED Logs before and after FR */

			helperMethodToValidateLedLogs(device, tapEnv, backupMap, testCaseId, BroadBandTestConstants.CONSTANT_7);

		} catch (Exception testException) {
			errorMessage = "Exception occurred while trying to validate resetting the device using SNMP v3: "
					+ testException.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		} finally {
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V2.toString());
			BroadBandSnmpUtils.checkSnmpIsUp(tapEnv, device);
			if (isFactoryReset) {
				LOGGER.info("### POST-CONDITION ### BEGIN BROAD BAND DEVICE REACTIVATION.");
				BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
				LOGGER.info("### POST-CONDITION ### END BROAD BAND DEVICE REACTIVATION.");
			}
			if (backupMap != null) {
				BroadBandPostConditionUtils.executePostCondtDeleteTemporaryFilesInGateway(device, tapEnv, 2, backupMap);
			}

			/**
			 * POST-CONDITION 2 : ENABLE THE PUBLIC WIFI
			 */

			BroadBandPostConditionUtils.executePostConditionToEnableOrDisablePublicWifiBasedOnStbProperty(device,
					tapEnv, BroadBandTestConstants.CONSTANT_2);

		}
	}

	/**
	 * Test to verify that the SNMP set/get can be performed successfully using
	 * valid MTA community string
	 * 
	 * <ol>
	 * <li>Step 1 : Verify MTA provisions on Gateway device</li>
	 * <li>Step 2 : Perform SNMP set and get for Telephone Line reset on the MTA IP
	 * with proper MTA Community String with interger 2</li>
	 * <li>Step 3 : Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB
	 * command output and check whether it meets the CableLabs specifications</li>
	 * <li>Step 4 : Verify Retrieving the Software version via WEBPA Parameter</li>
	 * <li>Step 5 : Verify Retrieving the manufacturer name via WEBPA Parameter</li>
	 * <li>Step 6 : Verify Retrieving the Model name via WEBPA Parameter</li>
	 * <li>Step 7 : Verify Retrieving the Boot Loader Version via WEBPA
	 * Parameter</li>
	 * <li>Step 8 : Verify Retrieving the Hardware Version via WEBPA Parameter</li>
	 * <li>Step 9 : Verify Retrieving the EMTA Address via SNMP and cross verify the
	 * value with the response retrieved via WEBPA Parameter</li>
	 * <li>Post Condition : Perform SNMP set and get for Telephone Line reset on the
	 * MTA IP with proper MTA Community String with interger 1</li>
	 * </ol>
	 * 
	 * @author Sathurya Ravi, Muthukumar Pachamuthu
	 * @refactor Athira
	 * 
	 * @param device The device to be used.
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1023")
	public void testSnmpGetSetOnMtaSide(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-123";
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;
		String snmpSetResponse = null;
		String snmpGetResponse = null;
		String mtaAddress = null;
		String snmpSystemDescrOutput = null;
		String softwareVersion = null;
		String manufacturerName = null;
		String manufacturerNameRetrievedViaWebpa = null;
		String modelName = null;
		String bootLoader = null;
		String hardwareVersion = null;
		String emtaAddress = null;
		BroadBandResultObject broadBandResultObject = null;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1023");
			LOGGER.info(
					"TEST DESCRIPTION: Validate that the device gives valid SNMP response with proper MTA community string");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("Step 1 : Verify MTA provisions on Gateway device");
			LOGGER.info(
					"Step 2 : Perform SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 2");
			LOGGER.info(
					"Step 3 : Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command output and check whether it meets the CableLabs specifications");
			LOGGER.info("Step 4 : Verify Retrieving the Software version via WEBPA Parameter");
			LOGGER.info("Step 5 : Verify Retrieving the manufacturer name via WEBPA Parameter");
			LOGGER.info("Step 6 : Verify Retrieving the Model name via WEBPA Parameter");
			LOGGER.info("Step 7 : Verify Retrieving the Boot Loader Version via WEBPA Parameter");
			LOGGER.info("Step 8 : Verify Retrieving the Hardware Version via WEBPA Parameter");
			LOGGER.info(
					"Step 9 : Verify Retrieving the EMTA Address via SNMP and cross verify the value with the response retrieved via WEBPA Parameter");
			LOGGER.info(
					"Post Condition : Perform SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 1");

			mtaAddress = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_COMMAND_MTA_IP_OF_DEVICE);
			if (CommonMethods.isNotNull(mtaAddress) && !mtaAddress.equals(BroadBandTestConstants.STRING_NULL_IP)) {
				/**
				 * Step 1 : Verify MTA provisions on Gateway device
				 */
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify MTA provisions on Gateway device");
				LOGGER.info("STEP " + stepNumber + " : ACTION : Check wehter the device has MTA provision");
				LOGGER.info("STEP " + stepNumber + " : EXPECTED : Device should have MTA provision.");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Device is not provisioned with MTA";
				mtaAddress = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_COMMAND_MTA_IP_OF_DEVICE);
				LOGGER.info("MTA ADDRESS :" + mtaAddress);
				status = CommonMethods.isNotNull(mtaAddress)
						&& !mtaAddress.equals(BroadBandTestConstants.STRING_NULL_IP);
				if (status) {
					LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully verified device has MTA provision");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

				/**
				 * Step 2 : Perform SNMP set and get for Telephone Line reset on the MTA IP with
				 * proper MTA Community String with integer 2
				 */
				stepNumber++;
				stepNum = "S" + stepNumber;
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION: Perform SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 2 ");
				LOGGER.info("STEP " + stepNumber
						+ " : ACTION: Execute SNMP set and get on the MIB(1.3.6.1.2.1.2.2.1.7.9) with interger 2 and proper MTA community string");
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED: SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 2 should be successful ");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Unable to perform SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 2";
				// Execute SNMP set on the MIB(1.3.6.1.2.1.2.2.1.7.9.0) with integer 2
				snmpSetResponse = BroadBandSnmpUtils.snmpSetOnEmta(tapEnv, device,
						BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_POSITIVE_SCENARIO.getOid(), SnmpDataType.INTEGER,
						BroadBandTestConstants.STRING_CONSTANT_2,
						BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_POSITIVE_SCENARIO.getTableIndex(), mtaAddress);
				// Verify the response from the SNMP Set is a Timeout response
				if (CommonMethods.isNotNull(snmpSetResponse)
						&& !(snmpSetResponse.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
						&& !(snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
						&& !(snmpSetResponse.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))
						&& (snmpSetResponse.contains(BroadBandTestConstants.SNMP_RESPONSE_FOR_TEL_LINE_STATUS_DOWN))) {
					errorMessage = "Seems like the device is not responding to proper Community String . ACTUAL : Response: "
							+ snmpSetResponse + errorMessage;
					// Execute SNMP get on the MIB(1.3.6.1.2.1.2.2.1.7.9)
					snmpGetResponse = BroadBandSnmpUtils.snmpGetOnEmta(tapEnv, device,
							BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_POSITIVE_SCENARIO.getOid(),
							BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_POSITIVE_SCENARIO.getTableIndex(), mtaAddress);
					// Verify the response from the SNMP get is a Timeout response
					status = CommonMethods.isNotNull(snmpGetResponse)
							&& !(snmpGetResponse.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
							&& !(snmpGetResponse.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
							&& !(snmpGetResponse.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))
							&& (snmpGetResponse
									.contains(BroadBandTestConstants.SNMP_RESPONSE_FOR_TEL_LINE_STATUS_DOWN));
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Successfully verified SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 2");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, ErrorType.SNMP + errorMessage, false);

				/**
				 * Step 3 : Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command
				 * output and check whether it meets the CableLabs specifications
				 */
				stepNumber++;
				stepNum = "S" + stepNumber;
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION : Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command output and check whether it meets the CableLabs specifications");
				LOGGER.info("STEP " + stepNumber
						+ " : ACTION : Execute the SNMP Get Command : snmpget -v2c -c <communitystring> udp6:<CM MAC> 1.3.6.1.2.1.1.1.0  to retrieve the SysDscr ");
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : Should return SysDescr with only FIVE standard  fields - SW_REV, MODEL, BOOTR, HW_REV and VENDOR. Should satisfy CableLabs specifications");
				LOGGER.info("**********************************************************************************");
				errorMessage = "SysDesc doesn't match the cablelabs specification with Five Standard Fields SW_REV, MODEL, BOOTR, HW_REV and VENDOR as expected";
				snmpSystemDescrOutput = BroadBandSnmpUtils.snmpGetOnEmta(tapEnv, device,
						BroadBandSnmpMib.ECM_SYS_DESCR.getOid(), BroadBandSnmpMib.ECM_SYS_DESCR.getTableIndex(),
						mtaAddress);
				LOGGER.info("ACTUAL : SNMP Response obtained for Command SysDescr is: " + snmpSystemDescrOutput);
				HashMap<String, String> systemDescriptor = BroadBandSnmpUtils
						.parseSystemDescriptorInformationFromSnmpOutput(snmpSystemDescrOutput);
				status = (systemDescriptor.size() == BroadBandTestConstants.ALLOWED_NUMBER_OF_SYS_DESCRIPTOR_FILED);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : SysDesc matches the cablelabs specification with Five Standard Fields SW_REV, MODEL, BOOTR, HW_REV and VENDOR as expected");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * Step 4 : Verify Retrieving the Software version via WEBPA Parameter
				 */
				stepNumber++;
				stepNum = "S" + stepNumber;
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION : Verify Retrieving the Software version via WEBPA Parameter: Device.DeviceInfo.SoftwareVersion and Cross verify with the Value Retrieved from the SNMP value in Step 3.");
				LOGGER.info("STEP " + stepNumber + " : ACTION : Execute WEBPA  command : "
						+ BroadBandWebPaConstants.TR69_PARAM_SOFTWARE_VERSION);
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : Software Version Retrieved from WEBPA should be same as the Software Version Value Retrived from SNMP in Step 3.");
				LOGGER.info("**********************************************************************************");
				softwareVersion = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_SOFTWARE_VERSION);
				LOGGER.info("Software Version Obtained via SNMP Command is :" + softwareVersion);
				errorMessage = "Unable to Obtain Software Version or Software Version Obtained via SNMP is null.";
				if (CommonMethods.isNotNull(softwareVersion)) {
					broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
							tapEnv, BroadBandWebPaConstants.TR69_PARAM_SOFTWARE_VERSION, softwareVersion,
							BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
							BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
					status = broadBandResultObject.isStatus();
					errorMessage = broadBandResultObject.getErrorMessage();
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Software Version Retrieved from WEBPA matches with the Software Version Value Retrived from SNMP in Step 3 as expected");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * Step 5 : Verify Retrieving the manufacturer name via WEBPA Parameter
				 */
				stepNumber++;
				stepNum = "S" + stepNumber;
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION : Verify Retrieving the manufacturer name via WEBPA Parameter: Device.DeviceInfo.Manufacturer and Cross verify with the Value Retrieved from the SNMP value in Step 3.");
				LOGGER.info("STEP " + stepNumber + " : ACTION : Execute WEBPA  command : "
						+ BroadBandWebPaConstants.TR69_PARAM_MANUFACTURER);
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : Manufacturer Name Retrieved from WEBPA should be same as the Manufacturer name Value Retrived from SNMP in Step 3.");
				LOGGER.info("**********************************************************************************");
				manufacturerName = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_VENDOR);
				LOGGER.info("Manufacturer Name Obtained via SNMP Command is :" + manufacturerName);
				errorMessage = "Unable to Obtain Manufacturer Name or Manufacturer Name Obtained via SNMP is null.";
				if (CommonMethods.isNotNull(manufacturerName)) {

					// manufacturerName obtained from property file for specific (fibre) devices
					try {

						manufacturerNameRetrievedViaWebpa = BroadbandPropertyFileHandler
								.getAutomaticsPropsValueByResolvingPlatform(device,
										BroadBandTestConstants.MANUFACTURERNAME_VIAWEBPA);

					} catch (Exception e) {
						manufacturerNameRetrievedViaWebpa = tapEnv.executeWebPaCommand(device,
								BroadBandWebPaConstants.TR69_PARAM_MANUFACTURER);
						LOGGER.info("manufacturerName Retrieved Via Webpa as no device specific value found");
					}

					LOGGER.info(
							"Manufacturer Name Obtained via WEBPA Command is :" + manufacturerNameRetrievedViaWebpa);
					errorMessage = "Unable to Obtain Manufacturer Name or Manufacturer Name Obtained via WEBPA is null.";
					if (CommonMethods.isNotNull(manufacturerNameRetrievedViaWebpa)) {
						errorMessage = "Manufacturer Name Retrieved from WEBPA doesn't match with the Manufacturer Name Value Retrived from SNMP in Step 3 as expected.";
						status = CommonUtils.patternSearchFromTargetString(manufacturerNameRetrievedViaWebpa,
								manufacturerName);
					}
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Manufacturer Name Retrieved from WEBPA matches with the Manufacturer name Value Retrived from SNMP in Step 3 as expected.");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * Step 6 : Verify Retrieving the Model name via WEBPA Parameter
				 */
				stepNumber++;
				stepNum = "S" + stepNumber;
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION : Verify Retrieving the Model name via WEBPA Parameter: Device.DeviceInfo.ModelName  and Cross verify with the Value Retrieved from the SNMP value in Step 3.");
				LOGGER.info("STEP " + stepNumber + " : ACTION : Execute WEBPA  command : "
						+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_MODELNAME);
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : Model Name Retrieved from WEBPA should be same as the Model name Value Retrived from SNMP in Step 3.");
				LOGGER.info("**********************************************************************************");
				modelName = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_MODEL);
				LOGGER.info("Model Name Obtained via SNMP Command is :" + modelName);
				errorMessage = "Unable to Obtain Model Name or Model Name Obtained via SNMP is null.";
				if (CommonMethods.isNotNull(modelName)) {
					broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
							tapEnv, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_MODELNAME, modelName,
							BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
							BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
					status = broadBandResultObject.isStatus();
					errorMessage = broadBandResultObject.getErrorMessage();
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Model Name Retrieved from WEBPA matches with the Model Name Value Retrived from SNMP in Step 3 as expected");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * Step 7 : Verify Retrieving the Boot Loader Version via WEBPA Parameter
				 */
				stepNumber++;
				stepNum = "S" + stepNumber;
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION : Verify Retrieving the Boot Loader Version via WEBPA Parameter: Device.DeviceInfo.X_CISCO_COM_BootloaderVersion  and Cross verify with the Value Retrieved from the SNMP value in Step 3.");
				LOGGER.info("STEP " + stepNumber + " : ACTION : \"Execute WEBPA  command : "
						+ BroadBandWebPaConstants.TR69_PARAM_DEVICE_INFO_BOOT_LOADER_VERSION);
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : Boot Loader Version Retrieved from WEBPA should be same as the Boot Loader Version Value Retrived from SNMP in Step 3.");
				LOGGER.info("**********************************************************************************");
				bootLoader = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_BOOT_LOADER_VERSION);
				LOGGER.info("Boot Loader Obtained via SNMP Command is :" + bootLoader);
				errorMessage = "Unable to Obtain Boot Loader or Boot Loader Obtained via SNMP is null.";
				if (CommonMethods.isNotNull(bootLoader)) {
					broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
							tapEnv, BroadBandWebPaConstants.TR69_PARAM_DEVICE_INFO_BOOT_LOADER_VERSION, bootLoader,
							BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
							BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
					status = broadBandResultObject.isStatus();
					errorMessage = broadBandResultObject.getErrorMessage();
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Boot Loader Retrieved from WEBPA matches with the Boot Loader Value Retrived from SNMP in Step 3 as expected");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * Step 8 : Verify Retrieving the Hardware Version via WEBPA Parameter
				 */
				stepNumber++;
				stepNum = "S" + stepNumber;
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION : Verify Retrieving the Hardware Version via WEBPA Parameter: Device.DeviceInfo.Hardwareversion  and Cross verify with the Value Retrieved from the SNMP value in Step 3.");
				LOGGER.info("STEP " + stepNumber + " : ACTION : Execute WEBPA  command : "
						+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_HARDWARE_VERSION);
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : Hardware Version Name Retrieved from WEBPA should be same as the Hardware version name Value Retrived from SNMP in Step 3.");
				LOGGER.info("**********************************************************************************");
				hardwareVersion = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_HARDWARE_VERSION);
				LOGGER.info("Hardware Version Obtained via SNMP Command is :" + hardwareVersion);
				errorMessage = "Unable to Obtain Hardware Version or Hardware Version Obtained via SNMP is null.";
				if (CommonMethods.isNotNull(hardwareVersion)) {
					broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
							tapEnv, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_HARDWARE_VERSION, hardwareVersion,
							BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
							BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
					status = broadBandResultObject.isStatus();
					errorMessage = broadBandResultObject.getErrorMessage();
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Hardware Version Retrieved from WEBPA matches with the Hardware Version Value Retrived from SNMP in Step 3 as expected");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * Step 9 : Verify Retrieving the EMTA Address via SNMP and cross verify the
				 * value with the response retrieved via WEBPA Parameter
				 */
				stepNumber++;
				stepNum = "S" + stepNumber;
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ " : DESCRIPTION : Verify Retrieving the EMTA Address  via SNMP using: 1.3.6.1.2.1.2.2.1.6.16 MIB and cross verify the value with the response retrieved via WEBPA Parameter: "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MTA_MAC);
				LOGGER.info("STEP " + stepNumber
						+ " : ACTION : Execute the SNMP Get Command : snmpget -v2c -c <communityString> udp6:<CM MAC> 1.3.6.1.2.1.2.2.1.6.16  to retrieve the Serial Number and Cross verify via WEBPA Command: "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MTA_MAC);
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : EMTA Address  Retrieved from SNMP should be same as the EMTA Address  Value Retrived via WEBPA.");
				LOGGER.info("**********************************************************************************");

				if (DeviceModeHandler.isBusinessClassDevice(device) || DeviceModeHandler.isFibreDevice(device)) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL: Not applicable for Fibre and bussiness class device types!");
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
				} else {

					emtaAddress = BroadBandSnmpUtils.executeSnmpGetOnRdkDevicesWithTableIndex(tapEnv, device,
							BroadBandSnmpMib.EMC_EMTA_ADDRESS.getOid(),
							BroadBandSnmpMib.EMC_EMTA_ADDRESS.getTableIndex());
					LOGGER.info("EMTA Address Obtained via SNMP Command is :" + emtaAddress);
					errorMessage = "Unable to Obtain EMTA Address or EMTA Address Filename Obtained via SNMP is null.";
					if (CommonMethods.isNotNull(emtaAddress)
							&& !(emtaAddress.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
							&& !(emtaAddress.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
							&& !(emtaAddress.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))) {
						try {
							broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(
									device, tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MTA_MAC, emtaAddress,
									BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
									BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
									BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
							status = broadBandResultObject.isStatus();
							errorMessage = broadBandResultObject.getErrorMessage();
						} catch (Exception e) {
							LOGGER.error("Exception occured while verifying EMTA Address : " + e.getMessage());
						}
					}
					if (status) {
						LOGGER.info("STEP " + stepNumber
								+ " : ACTUAL : MTA Address Retrieved from SNMP matches with the EMTA Address Value Retrived via WEBPA as expected.");
					} else {
						LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
					}
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
				}
			} else {
				stepNumber = 1;
				while (stepNumber <= 9) {
					stepNum = "s" + stepNumber;
					errorMessage = "STEP " + stepNumber
							+ ": ACTUAL : Device is not MTA provisioned so marking steps as NA.";
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNumber++;
				}
			}
		} catch (Exception exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			if (status) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				status = false;
				errorMessage = null;
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"POST-CONDITION 1 : DESCRIPTION: Perform SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 1 ");
				LOGGER.info(
						"POST-CONDITION 1 : ACTION: Execute SNMP set and get on the MIB( 1.3.6.1.2.1.2.2.1.7.9.0 ) with interger 1 and proper MTA community string");
				LOGGER.info("POST-CONDITION 1 : EXPECTED: SNMP set operation should be successful ");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Unable to perform SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 1";
				// Execute SNMP set on the MIB(1.3.6.1.2.1.2.2.1.7.9.0) with integer 1
				snmpSetResponse = BroadBandSnmpUtils.snmpSetOnEmta(tapEnv, device,
						BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_POSITIVE_SCENARIO.getOid(), SnmpDataType.INTEGER,
						BroadBandTestConstants.STRING_CONSTANT_1,
						BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_POSITIVE_SCENARIO.getTableIndex(), mtaAddress);
				LOGGER.info("SNMP command output for snmp set on MIB(1.3.6.1.2.1.2.2.1.7.9.0) : " + snmpSetResponse);
				// Verify the response from the SNMP Set is a Timeout response
				if (CommonMethods.isNotNull(snmpSetResponse)
						&& !(snmpSetResponse.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
						&& (snmpSetResponse.contains(BroadBandTestConstants.SNMP_RESPONSE_FOR_TEL_LINE_STATUS_UP))) {
					errorMessage = "Seems like the device is not responding to proper Community String . ACTUAL : Response: "
							+ snmpSetResponse + errorMessage;
					// Execute SNMP get on the MIB(1.3.6.1.2.1.2.2.1.7.9.0)
					snmpGetResponse = BroadBandSnmpUtils.snmpGetOnEmta(tapEnv, device,
							BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_POSITIVE_SCENARIO.getOid(),
							BroadBandSnmpMib.EMTA_TELEPHONE_LINE_RESET_POSITIVE_SCENARIO.getTableIndex(), mtaAddress);
					// Verify the response from the SNMP get is a Timeout response
					status = CommonMethods.isNotNull(snmpGetResponse)
							&& !(snmpGetResponse.trim()).contains(BroadBandTestConstants.SNMP_TIME_OUT_RESPONSE)
							&& (snmpGetResponse.contains(BroadBandTestConstants.SNMP_RESPONSE_FOR_TEL_LINE_STATUS_UP));
				}
				if (status) {
					LOGGER.info(
							"POST-CONDITION 1 : ACTUAL : Successfully verified SNMP set and get for Telephone Line reset on the MTA IP with proper MTA Community String with interger 1");
				} else {
					LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
				}
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1023");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Helper Method to validate Led logs before and after FR
	 * 
	 * @param device     Dut instance
	 * @param tapEnv     AutomaticsTapApi instance
	 * @param backupMap  Map<String, String> backUpMap
	 * @param testCaseId String testCaseId
	 * @param stepNum    String stepNum
	 * @refactor Athira
	 */
	public static void helperMethodToValidateLedLogs(Dut device, AutomaticsTapApi tapEnv, Map<String, String> backupMap,
			String testCaseId, int stepNum) {
		String stepNumber = "S" + stepNum;
		String errorMessage = "Unable to validate device led logs";
		boolean status = false;

		String pamSearchLog = null;
		String pamSearchLogBeforeFR = null;
		String pamSearchLogAfterBoot = null;
		String pamSearchLogAR = null;
		Boolean isSpecificDeviceledlogs = null;
		isSpecificDeviceledlogs = BroadbandPropertyFileHandler.isSpecificDeviceledlogsAvailable(device);

		String consoleLog = BroadBandCommandConstants.LOG_FILE_SEC_CONSOLE
				.replace(BroadBandCommandConstants.DIRECTORY_LOGS, BroadBandTestConstants.EMPTY_STRING);
		String pamLog = BroadBandCommandConstants.CMD_TO_GET_CONFIG_DOWNLOAD_DETAILS
				.replace(BroadBandCommandConstants.DIRECTORY_LOGS, BroadBandTestConstants.EMPTY_STRING);
		String currentPartnerIdName = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
		LOGGER.info("Current Partner ID of the device Retrieved via WEBPA is :" + currentPartnerIdName);

		try {
			pamSearchLog = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
					BroadBandTestConstants.PROP_KEY_PAMSEARCHLOG);
		} catch (Exception e) {
			pamSearchLog = BroadBandTraceConstants.LOG_MESSAGE_WHITE_BLINK_AFTER_BOOT_PAMLOG;
			LOGGER.info("pamSearchLog taking as" + BroadBandTraceConstants.LOG_MESSAGE_WHITE_BLINK_AFTER_BOOT_PAMLOG
					+ " as no device specific value found");
		}

		Boolean isDeviceledlogs = null;
		isDeviceledlogs = BroadbandPropertyFileHandler.isDeviceledlogsAvailable(device);

		if (isDeviceledlogs) {
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Validate for after reboot Device led logs in SecConsole.txt.0 and PAMlog.txt.0");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : check : SecConsole.txt.0 -> \"Alert! RDK Event will be processed once Device is online; Received WHITE Blink\" ,\"Changing Led to White\" ,PAMlog ->Front LED Transition: WHITE LED will blink, Reason: CaptivePortal_MODE");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Successfully validated Led device logs");
			LOGGER.info("**********************************************************************************");
			if (CommonMethods.isNotNull(currentPartnerIdName)
					&& BroadBandCommonUtils.verifySpecificPartnerAvailability(currentPartnerIdName)) {
				errorMessage = "THIS STEP IS NOT APPLICABLE FOR SPECIFIC PARTNER Mentioned in Porperty file";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": THIS STEP IS NOT APPLICABLE FOR SPECIFIC PARTNER Mentioned in Porperty file");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			} else {
				try {

					if (isSpecificDeviceledlogs) {

						status = BroadBandCommonUtils.doesFileExistWithinGivenTimeFrameInArm(tapEnv, device,
								BroadBandCommandConstants.CMD_TO_GET_CONFIG_DOWNLOAD_DETAILS,
								BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
								BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS)
								&& CommonMethods.isNotNull(CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
										pamSearchLog, BroadBandCommandConstants.CMD_TO_GET_CONFIG_DOWNLOAD_DETAILS,
										BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
										BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
					} else {

						try {

							pamSearchLogAR = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
									device, BroadBandTestConstants.PROP_KEY_PAMSEARCHALERT);
						} catch (Exception e) {
							pamSearchLogAR = BroadBandTraceConstants.LOG_MESSAGE_WHITE_BLINK_AFTER_BOOT_SECCONSOLE;
							LOGGER.info("pamSearchLog taking as"
									+ BroadBandTraceConstants.LOG_MESSAGE_WHITE_BLINK_AFTER_BOOT_SECCONSOLE
									+ " as no device specific value found");
						}

						status = BroadBandCommonUtils.doesFileExistWithinGivenTimeFrameInArm(tapEnv, device,
								BroadBandCommandConstants.LOG_FILE_SEC_CONSOLE,
								BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
								BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS)
								&& CommonMethods.isNotNull(CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
										pamSearchLogAR, BroadBandCommandConstants.LOG_FILE_SEC_CONSOLE,
										BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
										BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS))
								&& CommonMethods.isNotNull(CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
										BroadBandTraceConstants.LOG_MESSAGE_LED_CHANGE_WHITE_AFTER_BOOT_SECCONSOLE,
										BroadBandCommandConstants.LOG_FILE_SEC_CONSOLE,
										BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
										BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS))
								&& BroadBandCommonUtils.doesFileExistWithinGivenTimeFrameInArm(tapEnv, device,
										BroadBandCommandConstants.CMD_TO_GET_CONFIG_DOWNLOAD_DETAILS,
										BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
										BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS)
								&& CommonMethods.isNotNull(CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
										pamSearchLog, BroadBandCommandConstants.CMD_TO_GET_CONFIG_DOWNLOAD_DETAILS,
										BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
										BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

					}
				} catch (Exception e) {
					LOGGER.info("Exception caught while validating logs " + e.getMessage());
				}
				if (status) {
					status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device)
							&& BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				}
				if (status) {

					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Successfully verified log messages in SecConsole.txt.0 and PAMlog.txt.0 after device boots");
				} else {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}

				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			}

			stepNum++;
			stepNumber = "S" + stepNum;
			errorMessage = "Unable to validate device led logs";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Validate before reboot Device led logs in Backup SecConsole.txt.0 and PAMlog.txt.0 In nvram");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : check : BackupSecConsole.txt.0 -> \"Front LED Transition: Changing Led to Green\" ,\"Front LED Transition :  Mode -----> Blink\" ,BackupPAMlog.txt.0 ->\"Front LED Transition: GREEN LED will blink, Reason: Factory Reset\" ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Successfully validated Led device logs");
			LOGGER.info("**********************************************************************************");
			try {
				if (isSpecificDeviceledlogs) {

					try {
						pamSearchLog = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
								BroadBandTestConstants.PROP_KEY_PAMLOG);
					} catch (Exception e) {
						pamSearchLog = BroadBandTraceConstants.LOG_MESSAGE_LED_CHANGE_GREEN_BEFORE_FR_PAMLOG;
						LOGGER.info("pamSearchLog taking as"
								+ BroadBandTraceConstants.LOG_MESSAGE_LED_CHANGE_GREEN_BEFORE_FR_PAMLOG
								+ " as no device specific value found");
					}

					status = CommonMethods.isNotNull(CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
							pamSearchLog, backupMap.get(pamLog), BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
				} else {

					try {
						pamSearchLog = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
								BroadBandTestConstants.PROP_KEY_PAMLOG_FR);
						pamSearchLogBeforeFR = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
								device, BroadBandTestConstants.PROP_KEY_PAMLOG_BFR);

						pamSearchLogAfterBoot = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(
								device, BroadBandTestConstants.PROP_KEY_PAMLOG_POSTREBOOT);
					} catch (Exception e) {
						pamSearchLog = BroadBandTraceConstants.LOG_MESSAGE_LED_CHANGE_GREEN_BEFORE_FR_SECCONSOLE;

						pamSearchLogBeforeFR = BroadBandTraceConstants.LOG_MESSAGE_LED_MODE_BLINK_BEFORE_FR_SECCONSOLE;

						pamSearchLogAfterBoot = BroadBandTraceConstants.LOG_MESSAGE_LED_CHANGE_GREEN_BEFORE_FR_PAMLOG;

						LOGGER.info("No device specific logs found");
					}

					status = CommonMethods
							.isNotNull(CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, pamSearchLog,
									backupMap.get(consoleLog), BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
									BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS))
							&& CommonMethods.isNotNull(
									CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, pamSearchLogBeforeFR,
											backupMap.get(consoleLog), BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
											BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS))
							&& CommonMethods.isNotNull(
									CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, pamSearchLogAfterBoot,
											backupMap.get(pamLog), BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
											BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
				}
			} catch (Exception e) {
				LOGGER.info("Exception caught while validating logs " + e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Successfully verified log messages in SecConsole.txt.0 and PAMlog.txt.0 before device goes offline");
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		} else {
			errorMessage = "APPLICABLE ONLY FOR specific Devices";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : APPLICABLE ONLY FOR specific Devices");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);
			stepNum++;
			stepNumber = "S" + stepNum;
			errorMessage = "APPLICABLE ONLY FOR specific Devices";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : APPLICABLE ONLY FOR specific Devices");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);
		}

	}

	/**
	 * Helper method to backup seconsole and pamlog files in given path
	 * 
	 * @param Dut        device instance
	 * @param tapEnv     AutomaticsTapApi instance
	 * @param testCaseId String testCaseId
	 * @param stepNum    String stepNum
	 * @return Map<String, String> backUpMap
	 * @refactor Athira
	 */
	public static Map<String, String> helperMethodToBackupFiles(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
			int stepNum) {
		String stepNumber = "S" + stepNum;
		String errorMessage = "Unable to backup console log and pam log to nvram";
		boolean status = false;
		Map<String, String> backupMap = null;
		List<String> mustHaveLogFileList = new ArrayList<String>();
		String consoleLog = BroadBandCommandConstants.LOG_FILE_SEC_CONSOLE
				.replace(BroadBandCommandConstants.DIRECTORY_LOGS, BroadBandTestConstants.EMPTY_STRING);
		String pamLog = BroadBandCommandConstants.CMD_TO_GET_CONFIG_DOWNLOAD_DETAILS
				.replace(BroadBandCommandConstants.DIRECTORY_LOGS, BroadBandTestConstants.EMPTY_STRING);
		Boolean isDeviceledlogs = null;
		isDeviceledlogs = BroadbandPropertyFileHandler.isDeviceledlogsAvailable(device);

		if (isDeviceledlogs) {
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Backup SecConsole log and PAMlog to nvram");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute : tail -f /rdklogs/logs/SecConsole.txt.0 >> /nvram/BackupSecConsole.txt.0 ,Execute : tail -f /rdklogs/logs/Console.txt >> /nvram/PAMlog.txt.0");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Successfully backed up files to nvram");
			LOGGER.info("**********************************************************************************");
			try {
				mustHaveLogFileList.add(consoleLog);
				mustHaveLogFileList.add(pamLog);
				backupMap = BroadBandCommonUtils.verifyRdkLogAlbltyAndTailLogToGivenPathAndConsole(device, tapEnv,
						mustHaveLogFileList,
						CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.SYMBOL_PLUS,
								BroadBandTestConstants.STRING_VALUE_ONE),
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.ARM, CommonMethods.NVRAM_PATH);
				status = backupMap != null && !backupMap.isEmpty();
			} catch (Exception e) {
				LOGGER.error("Exception caught while backing up log files" + e.getMessage());
			}
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL : Successfully backed up SecConsole log and PAMlog to nvram");
			} else {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		} else {
			errorMessage = "APPLICABLE ONLY FOR Specific Devices";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : APPLICABLE ONLY for specific Devices");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);
		}

		return backupMap;
	}

	/**
	 * <li>"STEP 1. Verify SNMPv3 support is enabled using webpa"</li>
	 * <li>"STEP 2: Validate iptable for snmpv3 ports"</li>
	 * <li>"STEP 3: Validate ip6table for snmpv3 ports"</li>
	 * <li>"STEP 4: Validate certs availablity in gateway"</li>
	 * <li>"STEP 5: Validate signature and validity of rdk-managerV3.cert.pem"</li>
	 * <li>"STEP 6: Validate signature and validity of rdkv-snmpdV3.cert.pem"</li>
	 * <li>"STEP 7: Verify SNMPv3 on port 10161 to get 5GHz SSID name is working
	 * after enabling the SNMPv3"</li>
	 * <li>"STEP 8: Verify SNMPv3 on port 10163 to get 5GHz SSID name is working
	 * after enabling the SNMPv3 "</li>
	 * <li>"STEP 9: Get 2.4GHz Channel info using SNMPv2"</li>
	 * <li>"STEP 10: Verify SNMPv3 on port 10161 query for 2.4GHz Channel info after
	 * enabling the SNMPv3"</li>
	 * <li>"STEP 11: Verify SNMPv3 on port 10163 query for 2.4GHz Channel info after
	 * enabling the SNMPv3"</li>
	 * <li>"STEP 12: Get 2.4GHz SSID name using SNMPv2"</li>
	 * <li>"STEP 13: Verify SNMPv3 on port 10161 query for 2.4GHz SSID name after
	 * enabling the SNMPv3"</li>
	 * <li>"STEP 14: Verify SNMPv3 on port 10163 query for 2.4GHz SSID name after
	 * enabling the SNMPv3"</li>
	 * <li>"STEP 15: Get 5GHz wifi ssid status using SNMPv2"</li>
	 * <li>"STEP 16: Verify SNMPv3 query on port 10161 for 5GHz wifi ssid status
	 * after enabling the SNMPv3"</li>
	 * <li>"STEP 17: Verify SNMPv3 query for 5GHz wifi ssid status after enabling
	 * the SNMPv3"</li>
	 * <li>"STEP 18: Validate snmpv3 set command with old certs on 10161 port "</li>
	 * <li>"STEP 19: Validate snmpv3 set command with new certs on 10163 port "</li>
	 * 
	 * @author ArunKumar Jayachandran, Prasanth Reddy
	 * @refactor yamini.s
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMPV3-1001")
	public void testToVerifySnmpv3(Dut device) {

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMPV3-1001 ");
		LOGGER.info("TEST DESCRIPTION: Test to verify snmpv3 migration ");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("STEP 1. Verify SNMPv3 support is enabled using webpa");
		LOGGER.info("STEP 2: Validate iptable for snmpv3 ports");
		LOGGER.info("STEP 3: Validate ip6table for snmpv3 ports");
		LOGGER.info("STEP 4: Validate certs availablity in gateway");
		LOGGER.info("STEP 5: Validate signature and validity of rdk-managerV3.cert.pem");
		LOGGER.info("STEP 6: Validate signature and validity of rdkv-snmpdV3.cert.pem");
		LOGGER.info("STEP 7: Verify SNMPv3 on port 10161 to get 5GHz SSID name is working after enabling the SNMPv3");
		LOGGER.info("STEP 8: Verify SNMPv3 on port 10163 to get 5GHz SSID name is working after enabling the SNMPv3");
		LOGGER.info("STEP 9: Get 2.4GHz Channel info using SNMPv2");
		LOGGER.info("STEP 10: Verify SNMPv3 on port 10161 query for 2.4GHz Channel info after enabling the SNMPv3");
		LOGGER.info("STEP 11: Verify SNMPv3 on port 10163 query for 2.4GHz Channel info after enabling the SNMPv3");
		LOGGER.info("STEP 12: Get 2.4GHz SSID name using SNMPv2");
		LOGGER.info("STEP 13: Verify SNMPv3 on port 10161 query for 2.4GHz SSID name after enabling the SNMPv3");
		LOGGER.info("STEP 14: Verify SNMPv3 on port 10163 query for 2.4GHz SSID name after enabling the SNMPv3");
		LOGGER.info("STEP 15: Get 5GHz wifi ssid status using SNMPv2");
		LOGGER.info("STEP 16: Verify SNMPv3 query on port 10161 for 5GHz wifi ssid status after enabling the SNMPv3");
		LOGGER.info("STEP 17: Verify SNMPv3 query for 5GHz wifi ssid status after enabling the SNMPv3");
		LOGGER.info("STEP 18: Validate snmpv3 set command with old certs on 10161 port ");
		LOGGER.info("STEP 19: Validate snmpv3 set command with new certs on 10163 port ");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-SNMPV3-001";
		// Test step counter
		int stepCounter = 1;
		// Test step number
		String stepNumber = "s" + stepCounter;
		// String to store error message
		String errorMessage = "Webpa set failed for codebig first enable";
		// String to store response
		String response = null;
		// variable declaration ends
		String rdkManagerCert = null, rdkSnmpdCert = null;
		String snmpOutput = null;
		String expectedValue = null;
		try {

			LOGGER.info("################################# STARTING PRE-CONFIGURATIONS #############################");
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION 1: DESCRIPTION: Disable codebig first enable using webpa");
			LOGGER.info(
					"PRE-CONDITION 1: ACTION: Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CodeBigFirst.Enable as false");
			LOGGER.info("PRE-CONDITION 1: EXPECTED: Codebig first enable should be disabled using webpa");
			LOGGER.info("#####################################################################################");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_CODEBIG_FIRST_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : CODE BIG DISABLED SUCCESSFULLY ON DEVICE.");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : 1 FAILED : " + errorMessage);
			}
			LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter + ": DESCRIPTION: Verify SNMPv3 support is enabled using webpa ");
			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute webpa command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support");
			LOGGER.info("STEP " + stepCounter + ": EXPECTED: Webpa get request is success and parameter value is true");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SNMP.V3Support";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_SNMPV3_SUPPORT, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully verified the SNMPv3 support is enabled using webpa operation");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			errorMessage = "Unable to validate iptables";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Validate iptable for snmpv3 ports");

			LOGGER.info("STEP " + stepCounter + ": ACTION : Execute :iptables-save |grep 10161");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED : Output:-A INPUT -p udp -m udp -m multiport --dports 10161,10163 -j SNMP_FILTER  Successfully validated iptables");
			LOGGER.info("**********************************************************************************");
			try {
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_IPTABLES_SAVE_GREP_10161);
			} catch (Exception e) {
				LOGGER.error("Exception caught while executing command" + e.getMessage());
			}
			status = CommonMethods.isNotNull(response)
					&& CommonUtils.patternSearchFromTargetString(response, BroadBandCommandConstants.PORT_10161)
					&& CommonUtils.patternSearchFromTargetString(response, BroadBandCommandConstants.PORT_10163);
			if (status) {
				LOGGER.info(
						"STEP " + stepCounter + ": ACTUAL : Successfully validated both ports 10161,10163 in iptables");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			errorMessage = "Unable to validate ip6tables";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Validate ip6table for snmpv3 ports");

			LOGGER.info("STEP " + stepCounter + ": ACTION : Execute :ip6tables-save |grep 10161");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED : Output:-A INPUT -p udp -m udp -m multiport --dports 10161,10163 -j SNMP_FILTER Successfully validated iptables");
			LOGGER.info("**********************************************************************************");
			try {
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_IP6TABLES_SAVE_GREP_10161);
			} catch (Exception e) {
				LOGGER.error("Exception caught while executing command" + e.getMessage());
			}
			status = CommonMethods.isNotNull(response)
					&& CommonUtils.patternSearchFromTargetString(response, BroadBandCommandConstants.PORT_10161)
					&& CommonUtils.patternSearchFromTargetString(response, BroadBandCommandConstants.PORT_10163);

			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL : Successfully validated both ports 10161,10163 in ip6tables");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			errorMessage = "Unable to find certs in gateway";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Validate certs availablity in gateway");

			LOGGER.info("STEP " + stepCounter + ": ACTION : Execute :ls -la /etc/ssl/certs/snmp/tls/certs/*V3*");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED : Output:-rw-r--r--    1 root     root          5581 Jun  1 11:48 /etc/ssl/certs/snmp/tls/certs/rdk-managerV3.cert.pem  -rw-r--r--    1 root     root          5600 Jun  1 11:48 /etc/ssl/certs/snmp/tls/certs/rdkv-snmpdV3.cert.pem ");
			LOGGER.info("**********************************************************************************");

			try {
				rdkManagerCert = BroadbandPropertyFileHandler.getPropertyKeyForRDKManagerSNMPV3();
				rdkSnmpdCert = BroadbandPropertyFileHandler.getPropertyKeyForRDKBSnmpdSNMPV3();
			} catch (Exception e) {
				LOGGER.error("Stb props not available " + e.getMessage());
			}
			if (CommonMethods.isNotNull(rdkManagerCert) && CommonMethods.isNotNull(rdkSnmpdCert)) {
				try {
					response = tapEnv.executeCommandUsingSsh(device,
							CommonMethods.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
									BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
									BroadBandTestConstants.FILE_LOCATION_SNMP_CERTS));
				} catch (Exception e) {
					LOGGER.error("Exception caught while executing command" + e.getMessage());
				}
			}
			status = CommonMethods.isNotNull(response)
					&& CommonUtils.patternSearchFromTargetString(response, rdkManagerCert)
					&& CommonUtils.patternSearchFromTargetString(response, rdkSnmpdCert);

			if (status) {
				LOGGER.info("STEP " + stepCounter + ": ACTUAL : Successfully validated both certs are available");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			errorMessage = "Unable to read cert file";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION : Validate signature and validity of rdk-managerV3.cert.pem");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION : Execute :head /etc/ssl/certs/snmp/tls/certs/rdk-managerV3.cert.pem");
			LOGGER.info("STEP " + stepCounter + ": EXPECTED : Successfully validated certificate validatity ");
			LOGGER.info("**********************************************************************************");
			try {
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_HEAD,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.FILE_LOCATION_SNMP_CERTS, rdkManagerCert));
			} catch (Exception e) {
				LOGGER.error("Exception caught while executing command " + e.getMessage());
			}
			status = CommonMethods.isNotNull(response) && BroadBandSnmpUtils.validateSnmpv3Cert(response);

			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL : Successfully validated certificate encryption and validatity");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			errorMessage = "Unable to read cert file";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepCounter + ": DESCRIPTION : Validate signature and validity of rdkv-snmpdV3.cert.pem");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION : Execute :head /etc/ssl/certs/snmp/tls/certs/rdkv-snmpdV3.cert.pem");
			LOGGER.info("STEP " + stepCounter + ": EXPECTED : Successfully validated certificate validatity ");
			LOGGER.info("**********************************************************************************");
			try {
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_HEAD,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.FILE_LOCATION_SNMP_CERTS, rdkSnmpdCert));
			} catch (Exception e) {
				LOGGER.error("Exception caught while executing command " + e.getMessage());
			}
			status = CommonMethods.isNotNull(response) && BroadBandSnmpUtils.validateSnmpv3Cert(response);
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL :Successfully validated certificate encryption and validatity");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed get the system description response for SNMPv3 query after enabling the SNMPv3 support";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION: Verify SNMPv3 on port 10161 to get 5GHz SSID name is working after enabling the SNMPv3");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute command: snmpwalk -OQ -v 3 -t 10 --defSecurityModel=tsm -u RDK-SNMPV3-NMS -l authPriv -T our_identity=RDK-NMS -T their_identity=RDK-CPE dtlsudp6:[IPv6]:10161 .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV3 query should be successful and we should get system description ");
			LOGGER.info("******************************************************************************");
			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA1);
			((Device) device).setErouterIpAddress(
					CommonUtils.getDeviceIpAddressFromBhc(device, RDKBTestConstants.DEVICE_IP_ADDRESS_TYPE_ESTB));
			response = BroadBandSnmpUtils.performSnmpWalkUsingSnmpV3(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_SSID_5.getOid());
			if (BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, response)) {
				errorMessage = "SNMP response " + response + " retrived is not same as Webpa response for PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME;
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME, response);
			}
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received the response for SNMPv3 query after enabling SNMPv3 support on port 10161");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed get the system description response for SNMPv3 query after enabling the SNMPv3 support";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION: Verify SNMPv3 on port 10163 to get 5GHz SSID name is working after enabling the SNMPv3");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute command: snmpwalk -OQ -v 3 -t 10 --defSecurityModel=tsm -u RDK-SNMPV3-NMS -l authPriv -T our_identity=<CERT> -T their_identity=<CERT> dtlsudp6:[IPv6]:10163.1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV3 query should be successful and we should get system description ");
			LOGGER.info("******************************************************************************");
			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA256);
			response = BroadBandSnmpUtils.performSnmpWalkUsingSnmpV3(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_SSID_5.getOid());
			if (BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, response)) {
				errorMessage = "SNMP response " + response + " retrived is not same as Webpa response for PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME;
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME, response);
			}
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received the response for SNMPv3 query after enabling SNMPv3 on port 10163");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed to get the MOCA status using SNMPv2 after enabling SNMPv3 support";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter + ": DESCRIPTION: Get 2.4GHz Channel info using SNMPv2");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV2 query should be successful and we should get channel info");
			LOGGER.info("******************************************************************************");

			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V2.toString());
			response = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_2_4_CHANNEL_INFO.getOid());
			expectedValue = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS) ? BroadBandTestConstants.STRING_ZERO
							: tapEnv.executeWebPaCommand(device,
									BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ);
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response, expectedValue);
			LOGGER.info("STEP " + stepCounter + ": 2.4GHz channel info using SNMPv2: " + response);
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully verified Moca status using SNMPv2 after enabling SNMPv3 support");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed get the response for SNMPv3 query";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION: Verify SNMPv3 on port 10161 query for 2.4GHz Channel info after enabling the SNMPv3");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV3 query should be successful and we should get channel info");
			LOGGER.info("******************************************************************************");

			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA1);
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.performSnmpWalkUsingSnmpV3(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_2_4_CHANNEL_INFO.getOid());
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response, expectedValue);
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received 2.4GHz Channel info using SNMPv3 query on port 10161");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed get the response for SNMPv3 query";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION: Verify SNMPv3 on port 10163 query for 2.4GHz Channel info after enabling the SNMPv3");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV3 query should be successful and we should get channel info");
			LOGGER.info("******************************************************************************");

			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA256);
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.performSnmpWalkUsingSnmpV3(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_2_4_CHANNEL_INFO.getOid());
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response, expectedValue);
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received 2.4GHz Channel info using SNMPv3 query on port 10163");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed to get the 2.4GHz SSID name using SNMPv2  after enabling SNMPv3 support";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter + ": DESCRIPTION: Get 2.4GHz SSID name using SNMPv2");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 ");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV2 query should be successful and we should get 2.4GHz SSID name ");
			LOGGER.info("******************************************************************************");

			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V2.toString());
			snmpOutput = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid());
			response = BroadBandSnmpUtils.parseSnmpMibValueFromSnmpOutput(device, snmpOutput,
					BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid());
			if (BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, response)) {
				errorMessage = "SNMP response " + response + " retrived is not same as Webpa response for PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME;
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME, response);
			}
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received 2.4GHz SSID name using SNMPv2 command ");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed get the response for SNMPv3 query";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION: Verify SNMPv3 on port 10161 query for 2.4GHz SSID name after enabling the SNMPv3");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV3 query should be successful and we should get 2.4GHz SSID name");
			LOGGER.info("******************************************************************************");

			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA1);
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.performSnmpWalkUsingSnmpV3(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid());
			if (BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, response)) {
				errorMessage = "SNMP response " + response + " retrived is not same as Webpa response for PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME;
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME, response);
			}
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received 2.4GHz SSID name using SNMPv3 command on port 10161");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed get the response for SNMPv3 query";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION: Verify SNMPv3 on port 10163 query for 2.4GHz SSID name after enabling the SNMPv3");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV3 query should be successful and we should get 2.4GHz SSID name");
			LOGGER.info("******************************************************************************");

			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA256);
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.performSnmpWalkUsingSnmpV3(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_SSID_2_4.getOid());
			if (BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, response)) {
				errorMessage = "SNMP response " + response + " retrived is not same as Webpa response for PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME;
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME, response);
			}
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received 2.4GHz SSID name using SNMPv3 command on port 10163");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed to get the 5GHz wifi ssid status using SNMPv2  after enabling SNMPv3 support";
			response = null;
			expectedValue = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter + ": DESCRIPTION: Get 5GHz wifi ssid status using SNMPv2");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV2 query should be successful and we should get 5GHz wifi ssid status");
			LOGGER.info("******************************************************************************");

			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V2.toString());
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			snmpOutput = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_5_SSID_STATUS.getOid());
			response = BroadBandSnmpUtils.parseSnmpMibValueFromSnmpOutput(device, snmpOutput,
					BroadBandSnmpMib.ECM_WIFI_5_SSID_STATUS.getOid());
			expectedValue = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS) ? BroadBandTestConstants.STRING_CONSTANT_1
							: BroadBandTestConstants.STRING_CONSTANT_2;
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response, expectedValue);
			if (status) {
				LOGGER.info(
						"STEP " + stepCounter + ": ACTUAL: Successfully received 5GHz wifi ssid status using SNMPv2 ");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed get the response for SNMPv3 query";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION: Verify SNMPv3 query on port 10161 for 5GHz wifi ssid status after enabling the SNMPv3");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV3 query should be successful and we should get 5GHz wifi ssid status");
			LOGGER.info("******************************************************************************");

			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA1);
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.performSnmpWalkUsingSnmpV3(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_5_SSID_STATUS.getOid());
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response, expectedValue);
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received 5GHz wifi ssid status using SNMPv3 query on port 10161");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			status = false;
			errorMessage = "Failed get the response for SNMPv3 query";
			response = null;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION: Verify SNMPv3 query on port 10163 for 5GHz wifi ssid status after enabling the SNMPv3");

			LOGGER.info("STEP " + stepCounter
					+ ": ACTION: Execute snmpwalk command using oid .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101");
			LOGGER.info("STEP " + stepCounter
					+ ": EXPECTED: SnmpV3 query should be successful and we should get 5GHz wifi ssid status");
			LOGGER.info("******************************************************************************");

			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA256);
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());
			response = BroadBandSnmpUtils.performSnmpWalkUsingSnmpV3(tapEnv, device,
					BroadBandSnmpMib.ECM_WIFI_5_SSID_STATUS.getOid());
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response, expectedValue);
			if (status) {
				LOGGER.info("STEP " + stepCounter
						+ ": ACTUAL: Successfully received 5GHz wifi ssid status using SNMPv3 query on port 10163");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			errorMessage = "Failed get the response for SNMPv3 query";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION : Validate snmpv3 set command with old certs on 10161 port ");

			LOGGER.info("STEP " + stepCounter + ": ACTION : Execute snmpv3 set command");
			LOGGER.info("STEP " + stepCounter + ": EXPECTED : Successfully executed snmpv3 set command");
			LOGGER.info("**********************************************************************************");

			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA1);
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());

			String MibOid = BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getOid() + "."
					+ BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getTableIndex();

			response = BroadBandSnmpUtils.performSnmpSetUsingSnmpV3(tapEnv, device, MibOid, SnmpDataType.STRING,
					BroadBandTestConstants.TEST_SSID_2_4);

			if (BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, response)) {

				errorMessage = "SNMP set value " + BroadBandTestConstants.TEST_SSID_2_4
						+ " is not same as Webpa response for PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME;
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.TEST_SSID_2_4);
			}
			if (status) {
				LOGGER.info("STEP " + stepCounter + ": ACTUAL : Successfully executed snmpv3 on port 10161");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepCounter++;
			stepNumber = "s" + stepCounter;
			errorMessage = "Failed get the response for SNMPv3 query";
			status = false;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepCounter
					+ ": DESCRIPTION : Validate snmpv3 set command with new certs on 10163 port ");
			LOGGER.info("STEP " + stepCounter + ": ACTION : Execute snmpv3 set command");
			LOGGER.info("STEP " + stepCounter + ": EXPECTED : Successfully executed snmpv3 set command");
			LOGGER.info("**********************************************************************************");

			AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
					BroadBandTestConstants.SNMPV3_AUTH_SHA256);
			System.setProperty(SnmpConstants.SYSTEM_PARAM_SNMP_VERSION, SnmpProtocol.SNMP_V3.toString());

			MibOid = BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getOid() + "."
					+ BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getTableIndex();

			response = BroadBandSnmpUtils.performSnmpSetUsingSnmpV3(tapEnv, device, MibOid, SnmpDataType.STRING,
					BroadBandTestConstants.TEST_SSID_5);
			if (BroadBandSnmpUtils.hasNoSNMPErrorOnResponse(tapEnv, device, response)) {
				errorMessage = "SNMP set value " + BroadBandTestConstants.TEST_SSID_5
						+ " is not same as Webpa response for PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME;
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.TEST_SSID_5);
			}
			if (status) {
				AutomaticsPropertyUtility.setProperty(BroadBandTestConstants.SNMPV3_AUTH_MECHANISM,
						BroadBandTestConstants.SNMPV3_AUTH_SHA1);
				LOGGER.info("STEP " + stepCounter + ": ACTUAL : Successfully executed snmpv3 on port 10163");
			} else {
				LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying SNMPv3 control" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMPV3-1001");
	}

	/**
	 * * Test to reset MTA of the device
	 * 
	 * <ol>
	 * <li>STEP 1: Verify whether device has MTA provisioned by executing ifconfig
	 * mta0 command and checking the Ipv4 address is obtained</li>
	 * <li>STEP 2: Verify whether device has MTA IP and get it using Webpa
	 * Command</li>
	 * <li>STEP 3: Reset MTA module using SNMP command "snmpset -v2c -c <MTA
	 * COMMUNITY STRING> MTA IP 1.3.6.1.2.1.140.1.1.1.0 i 1"</li>
	 * </ol>
	 * 
	 * @param device
	 * @refactor Said Hisham
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-MTA-1001")
	public void testMtaReset(Dut device) {

		// Test case id
		String testId = "TC-RDKB-MTA-101";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// String to store webpa response
		String response = null;
		// stores the test result
		boolean result = false;
		try {

			LOGGER.info("#################### STARTING TEST CASE:TC-RDKB-MTA_1001 #####################");
			LOGGER.info("TEST DESCRIPTION: Test to reset MTA of the device");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"Step 1: Verify whether device has MTA provisioned by executing ifconfig mta0 command and checking the Ipv4 address is obtained");
			LOGGER.info("Step 2 : Verify whether device has MTA IP and get it using Webpa Command");
			LOGGER.info(
					"Step 3 : Reset MTA module using SNMP command snmpset -v2c -c <MTA COMMUNITY STRING> MTA IP 1.3.6.1.2.1.140.1.1.1.0 i 1");
			LOGGER.info("#####################################################################################");

			/**
			 * STEP 1:Verify whether device has MTA provisioned by executing 'ifconfig mta0'
			 * command and checking the Ipv4 address is obtained
			 */
			testStepNumber = "s1";
			result = false;
			LOGGER.info("######################################################################");
			LOGGER.info(
					"STEP 1 : DESCRIPTION: Verify whether device has MTA provisioned by executing 'ifconfig mta0' command and checking the Ipv4 address is obtained");
			LOGGER.info("STEP 1 : ACTION: execute 'ifconfig mta0'");
			LOGGER.info(
					"STEP 1 : EXPECTED: Device should have a valid Ip address with MTA interface properly intialized");

			if (!DeviceModeHandler.isBusinessClassDevice(device)) {

				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_IFCONFIG_MTA0);
				LOGGER.info("PATTERN: " + BroadBandTestConstants.INET_V4_ADDRESS_PATTERN);
				result = CommonMethods.isNotNull(response)
						&& CommonMethods.patternMatcher(response, BroadBandTestConstants.INET_V4_ADDRESS_PATTERN);
				errorMessage = "MTA interface is down so the MTA is not provisioned";
				if (result) {
					LOGGER.info(
							"STEP 1 : ACTUAL : successfully verified that mta0 interface is properly intialized with dhcp ipv4 address and the device is provisioned");
				} else {
					LOGGER.error("STEP 1 : ACTUAL : " + errorMessage);
				}
				LOGGER.info("######################################################################");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, result, errorMessage, true);
			} else {
				LOGGER.info(
						"Business Class devices has mta interface on CM Side. So this Step is Not applicable for Business Class models");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						BroadBandTestConstants.NOT_APPLICABLE_VALUE, false);
			}
			/**
			 * STEP 2:Get MTA IP from the device using WEBPA command
			 */
			testStepNumber = "s2";
			result = false;
			LOGGER.info("######################################################################");
			LOGGER.info("STEP 2 : DESCRIPTION : Verify whether device has MTA IP and get it using Webpa Command");
			LOGGER.info("STEP 2 : ACTION : execute WebPA command 'Device.X_CISCO_COM_MTA.IPAddress'");
			LOGGER.info("STEP 2 : EXPECTED : Device should be configured with MTA ");

			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_COMMAND_MTA_IP_OF_DEVICE);
			result = CommonMethods.isNotNull(response) && !response.equals(BroadBandTestConstants.STRING_NULL_IP);
			errorMessage = "Unable to get Webpa response from the device";
			if (result) {
				LOGGER.info("STEP 2 : ACTUAL : Webpa response is successful MTA IP of device is:" + response);
			} else {
				LOGGER.error("STEP 2 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("######################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, result, errorMessage, true);

			/**
			 * STEP 3:Reset MTA through SNMP
			 */
			testStepNumber = "s3";
			result = false;
			LOGGER.info("######################################################################");
			LOGGER.info("STEP 3 : DESCRIPTION : Reset MTA through SNMP");
			LOGGER.info(
					"STEP 3 : ACTION : execute command snmpset -v 2c  -c <MTA COMMUNITY> -t 10 -OQ udp:<MATA IP>:161 1.3.6.1.2.1.140.1.1.1.0 i 1");
			LOGGER.info("STEP 3 : EXPECTED : Device MTA should reset and SNMP response should be successful");

			String mtaIpAddress = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_COMMAND_MTA_IP_OF_DEVICE);

			response = BroadBandSnmpUtils.snmpSetOnEmta(tapEnv, device, BroadBandTestConstants.STRING_MTA_RESET_OID,
					SnmpDataType.INTEGER, BroadBandTestConstants.STRING_VALUE_ONE, BroadBandTestConstants.STRING_ZERO,
					mtaIpAddress);

			result = CommonMethods.isNotNull(response) && response.equals(BroadBandTestConstants.STRING_VALUE_ONE);
			errorMessage = "Reset MTA through is unsuccessful; Response:" + response;
			if (result) {
				LOGGER.info("STEP 3 : ACTUAL : Reset MTA through SNMP is successful");
			} else {
				LOGGER.error("STEP 3 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("######################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, result, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			result = false;
			LOGGER.error("Unable to reset MTA of the device");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, result, errorMessage, false);
		}
	}

	/**
	 * Validate the SSID name and BSSID of the Publicwifi
	 * <ol>
	 * <li>1. Perform factory reset on the unit via WebPa</li>
	 * <li>2. Cross validate the 2.4 GHz BSSID Mac via webpa</li>
	 * <li>3. Cross validate the 5 GHz BSSID Mac via webpa</li>
	 * <li>4. Get the SSID of the 2.4 GHz public wifi via webpa</li>
	 * <li>5. Get the SSID of the 5 GHz public wifi via webpa</li>
	 * <li>6. Enable public wifi on the device</li>
	 * <li>7. Get the SSID of the 2.4 GHz public wifi via webpa</li>
	 * <li>8. Get the SSID of the 5 GHz public wifi via webpa</li>
	 * </ol>
	 * 
	 * @refactor Govardhan
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-PUBLIC-WIFI-CONFIG-1001")
	public void testVerifyPublicWifiMibs(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-PUBLIC-WIFI-CONFIG-001";
		int stepNumber = 1;
		String stepNum = null;
		String errorMessage = null;
		String bssid_24 = null;
		String bssid_5 = null;
		String response = null;
		boolean status = false;
		boolean isFactoryReset = false;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PUBLIC-WIFI-CONFIG-1001");
		LOGGER.info("TEST DESCRIPTION: Validate the SSID name and BSSID of the Publicwifi");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Perform factory reset on the unit via WebPa ");
		LOGGER.info("2. Cross validate the 2.4 GHz BSSID Mac via webpa ");
		LOGGER.info("3. Cross validate the 5 GHz BSSID Mac via webpa ");
		LOGGER.info("4. Get the SSID of the 2.4 GHz public wifi via webpa ");
		LOGGER.info("5. Get the SSID of the 5 GHz public wifi via webpa ");
		LOGGER.info("6. Enable public wifi on the device ");
		LOGGER.info("7. Get the SSID of the 2.4 GHz public wifi via webpa ");
		LOGGER.info("8. Get the SSID of the 5 GHz public wifi via webpa ");

		LOGGER.info("#######################################################################################");

		try {
			// STEP 1 : Perform factory reset on the unit via WebPa
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to factory reset via WebPa has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Perform factory reset on the unit via WebPa");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute WebPa SET command on the object "
					+ "Device.X_CISCO_COM_DeviceControl.FactoryReset using the command,"
					+ " \"curl --request SET --url \"<WEBPA_URL>:<ECM MAC>/config?names"
					+ "=Device.X_CISCO_COM_DeviceControl.FactoryReset\" "
					+ "--header \"authorization: Bearer <SAT-TOKEN>\"\"");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : The parameter should get set successfully and return a 200 success response");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
			isFactoryReset = status;
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Attempt to factory reset via WebPa is successful");
				LOGGER.info("Waiting for a minute..");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// STEP 2 : Cross validate the 2.4 GHz BSSID Mac via webpa
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to retrieve 2.4 GHz public wifi BSSID Mac has failed via webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION :Verify the 2.4 GHz BSSID Mac via webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute WebPa GET command on the object Device.WiFi.SSID.10003.BSSID"
					+ " using the command, \"curl --request GET --url " + "'<WEBPA_URL>:<ECM MAC>/config?names="
					+ "Device.WiFi.SSID.10003.BSSID ' --header 'authorization: Bearer <SAT-TOKEN>'\"");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : The BSSID from webpa should be retieved sucessfully ");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PUBLIC_WIFI_SSID_BSSID
							.replace(BroadBandTestConstants.TR181_NODE_REF, WEBPA_AP_INDEXES.PUBLIC_WIFI.get24Ghz()));
			LOGGER.info("2.4 GHz Public wifi BSSID Mac = " + response);
			status = CommonMethods.isNotNull(response) && CommonMethods.isMacValid(response);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : The 2.4 GHz BSSID Mac address for public wifi is as expected");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// STEP 3 : Cross validate the 5 GHz BSSID Mac via webpa
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to retrieve 5 GHz public wifi BSSID Mac has failed via webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify 5 GHz BSSID Mac via webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute WebPa GET command on the object Device.WiFi.SSID.10103.BSSID"
					+ " using the command, \"curl --request GET --url " + "'<WEBPA_URL>:<ECM MAC>/config?names="
					+ "Device.WiFi.SSID.10103.BSSID ' --header 'authorization: Bearer <SAT-TOKEN>'\"");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : The BSSID from webpa should be retieved sucessfully");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PUBLIC_WIFI_SSID_BSSID
							.replace(BroadBandTestConstants.TR181_NODE_REF, WEBPA_AP_INDEXES.PUBLIC_WIFI.get5Ghz()));
			LOGGER.info("5 GHz Public wifi BSSID Mac = " + response);
			status = CommonMethods.isNotNull(response) && CommonMethods.isMacValid(response);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL : The 5 GHz BSSID Mac address for public wifi is as expected");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// STEP 4 : Get the SSID of the 2.4 GHz public wifi via webpa
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to retrieve 2.4 GHz public wifi SSID has failed via webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Get the SSID of the 2.4 GHz public wifi via webpa");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute the following webpa command"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID);
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : The following response should be received. : \"OutOfService\"");
			LOGGER.info("**********************************************************************************");
			String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID);
			LOGGER.info("2.4GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
			status = CommonMethods.isNotNull(ssidNameFromWebPa)
					&& ssidNameFromWebPa.equals(BroadBandTestConstants.OUTOFSERVICE);
			if (status) {
				bssid_24 = response;
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : The 2.4 GHz SSID for public wifi is as expected");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// STEP 5 : Get the SSID of the 5 GHz public wifi via webpa
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to retrieve 5 GHz public wifi SSID has failed via webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Get the SSID of the 5 GHz public wifi via SNMP");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute WebPa GET command on the object Device.WiFi.SSID.10003.BSSID");
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : The following response should be received. \"OutOfService\"");
			LOGGER.info("**********************************************************************************");
			ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID);
			LOGGER.info("5GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
			status = CommonMethods.isNotNull(ssidNameFromWebPa)
					&& ssidNameFromWebPa.equals(BroadBandTestConstants.OUTOFSERVICE);
			if (status) {

				LOGGER.info("STEP " + stepNumber + ": ACTUAL : The 5 GHz SSID for public wifi is as expected");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// STEP 6 : Enable public wifi on the device
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to enable public wifi on device has failed";
			status = false;
			BroadBandResultObject resultObject = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Enable public wifi on the device ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Enable public wifi on the device via webpa");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : public wifi should get enabled successfuly");
			LOGGER.info("**********************************************************************************");

			List<WebPaParameter> webPaParameters = BroadBandWebPaUtils.getListOfWebpaParametersForBothPublicWifis();
			resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
					webPaParameters);
			status = resultObject.isStatus();
			errorMessage = resultObject.getErrorMessage();

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Attempt to enable public wifi on device is successful");
				LOGGER.info("Waiting for 60 seconds for the changes to take effect");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// STEP 7 : Get the SSID of the 2.4 GHz public wifi via webpa
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to retrieve 2.4 GHz public wifi SSID has failed via SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Get the SSID of the 2.4 GHz public wifi via webpa");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute the following webpa command"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID);
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : The following response should be received. : \"OutOfService\"");
			LOGGER.info("**********************************************************************************");
			ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID);
			LOGGER.info("2.4GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
			status = CommonMethods.isNotNull(ssidNameFromWebPa)
					&& ssidNameFromWebPa.equals(BroadBandTestConstants.OUTOFSERVICE);

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : The 2.4 GHz SSID for public wifi is as expected");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// STEP 8 : Get the SSID of the 5 GHz public wifi via webpa
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to retrieve 5 GHz public wifi SSID has failed via SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Get the SSID of the 5 GHz public wifi via SNMP");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute WebPa GET command on the object Device.WiFi.SSID.10003.BSSID");
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : The following response should be received. \"OutOfService\"");
			LOGGER.info("**********************************************************************************");
			ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
					AutomaticsTapApi.getSTBPropsValue(BroadBandPropertyKeyConstants.PROP_KEY_PUBLIC_WIFI_SSID_5));
			LOGGER.info("5GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
			status = CommonMethods.isNotNull(ssidNameFromWebPa)
					&& ssidNameFromWebPa.equals(AutomaticsTapApi.getSTBPropsValue(BroadBandPropertyKeyConstants.PROP_KEY_PUBLIC_WIFI_SSID_5));

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : The 5 GHz SSID for public wifi is as expected");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);

		} finally {

			if (isFactoryReset) {
				LOGGER.info("### POST-CONDITION ### BEGIN BROAD BAND DEVICE REACTIVATION.");
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
						BroadBandTestConstants.CONSTANT_1);
				LOGGER.info("### POST-CONDITION ### END BROAD BAND DEVICE REACTIVATION.");
			}
			LOGGER.info("ENDING TEST CASE: TC-RDKB-PUBLIC-WIFI-CONFIG-1001");
			LOGGER.info("#######################################################################################");
		}
	}

	/**
	 * Test to verify 2.4 GHz & 5 GHz SSID of Private Wi-Fi, Public Wi-Fi and
	 * Passphrase of Private Wi-Fi can be retrieved successfully through SNMP v2
	 * Queries
	 * 
	 * <ol>
	 * <li>STEP 1: Execute SNMP v2 GET command on OID
	 * (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001) to get the value of 2.4 GHz private
	 * Wi-Fi SSID</li>
	 * <li>STEP 2: Execute SNMP v2 GET command on OID
	 * (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101) to get the value of 5 GHz private
	 * Wi-Fi SSID</li>
	 * <li>STEP 3: Execute TR181 Device.WiFi.SSID.10003.SSID to get the value of 2.4
	 * GHz Public Wi-Fi SSID</li>
	 * <li>STEP 4: Execute TR181 Device.WiFi.SSID.10103.SSID to get the value of 5
	 * GHz Public Wi-Fi SSID</li>
	 * <li>STEP 5: Execute SNMP v2 GET command on
	 * OID(1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001) to get the value of 2.4 GHz
	 * private Wi-Fi Passphrase</li>
	 * <li>STEP 6: Execute SNMP v2 GET command on
	 * OID(1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101) to get the value of 5 GHz private
	 * Wi-Fi Passphrase</li>
	 * </ol>
	 * 
	 * @param device The device to be used.
	 * @author SATHYA KISHORE
	 * @refactor Athira
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1018")
	public void testSnmpv2GetOnPrivateWifiSsid(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-118";
		String stepNumber = "s1";
		boolean status = false; // stores the test status
		String errorMessage = null; // stores the error message
		boolean isPublicWifiEnabled = false;

		try {
			BroadBandResultObject result = null; // stores test result and error message
			ExecutionStatus testStatus = ExecutionStatus.FAILED; // stores the execution status
			String systemCommand = null;
			String webPaOutput = null; // stores WebPA output

			/**
			 * Step 1 : Execute SNMP v2 GET command on OID
			 * (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001) to get the value of 2.4 GHz private
			 * Wi-Fi SSID
			 * 
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: Execute SNMP v2 GET command on OID (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001)  to get the value of  2.4 GHz private Wi-Fi SSID");
			LOGGER.info("EXPECTED: The private Wi-Fi SSID of 2.4GHz Radio should be retrieved using SNMP v2 Query");
			LOGGER.info("**********************************************************************************");
			systemCommand = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv, BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandTestConstants.PRIVATE_WIFI_TYPE, BroadBandTestConstants.SSID_PARAM);
			result = BroadBandSnmpUtils.retrieveSnmpOutputAndVerifyWithSystemCommandOutput(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getTableIndex(), systemCommand,
					"private Wi-Fi SSID of 2.4GHz");
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			testStatus = result.getExecutionStatus();
			LOGGER.info("S1 ACTUAL: "
					+ (status ? "The private Wi-Fi SSID of 2.4GHz Radio can be retrieved using SNMP v2 Query"
							: errorMessage));
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 2 : Execute SNMP v2 GET command on OID
			 * (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101) to get the value of 5 GHz private
			 * Wi-Fi SSID
			 * 
			 */
			stepNumber = "s2";
			status = false;
			testStatus = ExecutionStatus.FAILED;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: Execute SNMP v2 GET command on OID (1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101) to get the value of 5 GHz private Wi-Fi SSID");
			LOGGER.info("EXPECTED: The private Wi-Fi SSID of 5GHz Radio should be retrieved using SNMP v2 Query");
			LOGGER.info("**********************************************************************************");
			systemCommand = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv, BroadBandTestConstants.BAND_5GHZ,
					BroadBandTestConstants.PRIVATE_WIFI_TYPE, BroadBandTestConstants.SSID_PARAM);
			result = BroadBandSnmpUtils.retrieveSnmpOutputAndVerifyWithSystemCommandOutput(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getTableIndex(), systemCommand,
					"private Wi-Fi SSID of 5GHz");
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			testStatus = result.getExecutionStatus();
			LOGGER.info("S2 ACTUAL: "
					+ (status ? "The private Wi-Fi SSID of 5 GHz Radio can be retrieved using SNMP v2 Query"
							: errorMessage));
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 3 : Execute webpa Device.WiFi.SSID.10003.SSID to get the value of 2.4
			 * GHz Public Wi-Fi SSID
			 * 
			 */
			stepNumber = "s3";
			status = false;
			testStatus = ExecutionStatus.FAILED;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: Execute TR181 Device.WiFi.SSID.10003.SSID to get the value of 2.4 GHz Public Wi-Fi SSID");
			LOGGER.info("EXPECTED: The Public Wi-Fi SSID of 2.4GHz Radio should be retrieved using webpa");
			LOGGER.info("**********************************************************************************");
			// Enable 2.4 GHz Public Wifi
			LOGGER.info("### Enabling 2.4 GHz Public Wi-Fi ###");
			status = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS)
					&& BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ENABLE_STATUS);
			errorMessage = "Unable to check 2.4 Ghz SSID of Public Wi-Fi using SNMP v2, since Public Wi-Fi can not be enabled";
			testStatus = ExecutionStatus.NOT_TESTED;
			isPublicWifiEnabled = status;
			if (status) {
				status = false;
				String ssidRetrievedFromSystemCommand = null;
				tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
				systemCommand = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv, BroadBandTestConstants.BAND_2_4GHZ,
						BroadBandTestConstants.PUBLIC_WIFI_TYPE, BroadBandTestConstants.SSID_PARAM);
				ssidRetrievedFromSystemCommand = BroadBandWebPaUtils.getSsidNameRetrievedUsingDeviceCommand(device,
						tapEnv, systemCommand);
				status = CommonMethods.isNotNull(ssidRetrievedFromSystemCommand);
				errorMessage = "Using system command unable to retrieve " + BroadBandTestConstants.SSID_PARAM
						+ " Radio";
				if (status) {

					status = false;
					String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID);
					LOGGER.info("2.4GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
					status = CommonMethods.isNotNull(ssidNameFromWebPa) && CommonUtils
							.patternSearchFromTargetString(ssidRetrievedFromSystemCommand, ssidNameFromWebPa);
					testStatus = status ? ExecutionStatus.PASSED : ExecutionStatus.FAILED;
					errorMessage = "webpa response failed for Device.WiFi.SSID.10003.SSID :" + ssidNameFromWebPa;
				}
			}

			LOGGER.info("S3 ACTUAL: "
					+ (status ? "The Public Wi-Fi SSID of 2.4GHz Radio can be retrieved using webpa" : errorMessage));
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 4 : Execute TR181 Device.WiFi.SSID.10103.SSID to get the value of 5 GHz
			 * Public Wi-Fi SSID
			 * 
			 */
			stepNumber = "s4";
			status = false;
			testStatus = ExecutionStatus.FAILED;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: Execute TR181 Device.WiFi.SSID.10003.SSID to get the value of 5 GHz Public Wi-Fi SSID");
			LOGGER.info("EXPECTED: The Public Wi-Fi SSID of 5GHz Radio should be retrieved using webpa");
			LOGGER.info("**********************************************************************************");
			// Enable 5 GHz Public Wifi
			LOGGER.info("### Enabling 5 GHz Public Wi-Fi ###");
			status = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS)
					&& BroadBandWebPaUtils.checkAndEnableSSIDForGivenInterface(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ENABLE_STATUS);
			errorMessage = "Unable to check 5 Ghz SSID of Public Wi-Fi using SNMP v2, since Public Wi-Fi can not be enabled";
			testStatus = ExecutionStatus.NOT_TESTED;
			isPublicWifiEnabled = (isPublicWifiEnabled) ? isPublicWifiEnabled : status;
			if (status) {
				status = false;
				String ssidRetrievedFromSystemCommand = null;
				tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
				systemCommand = BroadBandWebPaUtils.getWiFiInterface(device, tapEnv, BroadBandTestConstants.BAND_5GHZ,
						BroadBandTestConstants.PUBLIC_WIFI_TYPE, BroadBandTestConstants.SSID_PARAM);
				ssidRetrievedFromSystemCommand = BroadBandWebPaUtils.getSsidNameRetrievedUsingDeviceCommand(device,
						tapEnv, systemCommand);
				status = CommonMethods.isNotNull(ssidRetrievedFromSystemCommand);
				errorMessage = "Using system command unable to retrieve " + BroadBandTestConstants.SSID_PARAM
						+ " Radio";
				if (status) {

					status = false;
					String ssidNameFromWebPa = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID);
					LOGGER.info("5GHz Public SSID name retrieved using WebPa =" + ssidNameFromWebPa);
					status = CommonMethods.isNotNull(ssidNameFromWebPa) && CommonUtils
							.patternSearchFromTargetString(ssidRetrievedFromSystemCommand, ssidNameFromWebPa);
					testStatus = status ? ExecutionStatus.PASSED : ExecutionStatus.FAILED;
					errorMessage = "webpa response failed for Device.WiFi.SSID.10103.SSID :" + ssidNameFromWebPa;
				}
			}
			LOGGER.info("S4 ACTUAL: "
					+ (status ? "The Public Wi-Fi SSID of 5 GHz Radio can be retrieved using webpa" : errorMessage));
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 5 : Execute SNMP v2 GET command on OID
			 * (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10001) to get the value of 2.4 GHz private
			 * Wi-Fi Passphrase
			 * 
			 */
			stepNumber = "s5";
			status = false;
			testStatus = ExecutionStatus.FAILED;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: Execute SNMP v2 GET command on OID (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10001) to get the value of  2.4 GHz private Wi-Fi Passphrase");
			LOGGER.info(
					"EXPECTED: The private Wi-Fi Passphrase of 2.4GHz Radio should be retrieved using SNMP v2 Query");
			LOGGER.info("**********************************************************************************");
			result = BroadBandSnmpUtils.retrieveSnmpOutputAndVerifyWithWebPaOutput(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getTableIndex(),
					WebPaParamConstants.WEBPA_PARAM_SSID_PASSWORD_2_4GHZ, "private Wi-Fi Passphrase of 2.4GHz");
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			testStatus = result.getExecutionStatus();
			LOGGER.info("S5 ACTUAL: "
					+ (status ? "The private Wi-Fi Passphrase of 2.4GHz Radio can be retrieved using SNMP v2 Query"
							: errorMessage));
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

			/**
			 * Step 6 : Execute SNMP v2 GET command on OID
			 * (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101) to get the value of 5 GHz private
			 * Wi-Fi Passphrase
			 * 
			 */
			stepNumber = "s6";
			status = false;
			testStatus = ExecutionStatus.FAILED;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: Execute SNMP v2 GET command on OID (1.3.6.1.4.1.17270.50.2.2.3.1.1.2.10101) to get the value of 5 GHz private Wi-Fi Passphrase");
			LOGGER.info("EXPECTED: The private Wi-Fi Passphrase of 5GHz Radio should be retrieved using SNMP v2 Query");
			LOGGER.info("**********************************************************************************");
			result = BroadBandSnmpUtils.retrieveSnmpOutputAndVerifyWithWebPaOutput(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_PASSPHRASE.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_PASSPHRASE.getTableIndex(),
					WebPaParamConstants.WEBPA_PARAM_SSID_PASSWORD_5GHZ, "private Wi-Fi Passphrase of 2.4GHz");
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			testStatus = result.getExecutionStatus();
			LOGGER.info("S6 ACTUAL: "
					+ (status ? "The private Wi-Fi Passphrase of 5 GHz Radio can be retrieved using SNMP v2 Query"
							: errorMessage));
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, testStatus, errorMessage, false);

		} catch (Exception testException) {
			errorMessage = "Exception occurred while retrieving Wi-Fi Parameters using SNMP v2: "
					+ testException.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		} finally {
			if (isPublicWifiEnabled) {
				BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI,
						BroadBandTestConstants.FALSE, BroadBandTestConstants.CONSTANT_3);
			}
		}
	}

	/**
	 * Verify SNMP configuration fo Wifi
	 * <ol>
	 * <li>Verify retrieving the configuration of channel selection for 2.4Ghz using
	 * SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000)</li>
	 * <li>Verify setting the invalid channel selection value as 12 for 2.4Ghz using
	 * SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000)</li>
	 * <li>Verify retrieving the configuration of channel selection for 5Ghz using
	 * SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100)</li>
	 * <li>Verify setting the Invalid channel selection value as 38 for 5Ghz using
	 * SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100)</li>
	 * <li>Verify retrieving the configuration of Max Concurrent Client Limit on 2.4
	 * Ghz using Device.WiFi.AccessPoint.10003.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify setting the minimum value for Max Concurrent Client Limit on 2.4
	 * Ghz using Device.WiFi.AccessPoint.10003.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify setting the maximum value for Max Concurrent Client Limit on 2.4
	 * Ghz using Device.WiFi.AccessPoint.10003.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify setting the invalid minimum value for Max Concurrent Client Limit
	 * on 2.4 Ghz using Device.WiFi.AccessPoint.10003.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify setting the invalid maximum value for Max Concurrent Client Limit
	 * on 2.4 Ghz using Device.WiFi.AccessPoint.10003.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify retrieving the configuration of Max Concurrent Client Limit on
	 * 5Ghz using Device.WiFi.AccessPoint.10103.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify setting the minimum value for Max Concurrent Client Limit on 5Ghz
	 * using Device.WiFi.AccessPoint.10103.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify setting the maximum value for Max Concurrent Client Limit on 5Ghz
	 * using Device.WiFi.AccessPoint.10103.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify setting the invalid minimum value for Max Concurrent Client Limit
	 * on 5Ghz using Device.WiFi.AccessPoint.10103.X_CISCO_COM_BssMaxNumSta</li>
	 * <li>Verify setting the invalid minimum value for Max Concurrent Client Limit
	 * on 5Ghz using Device.WiFi.AccessPoint.10103.X_CISCO_COM_BssMaxNumSta)</li>
	 * <li>Verify retrieving the configuration of remote id and circuit id using
	 * SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)</li>
	 * <li>Verify setting the minimum value for remote id and circuit id using SNMP
	 * OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)</li>
	 * <li>Verify setting the Maximum value for remote id and circuit id using SNMP
	 * OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)</li>
	 * <li>Verify setting the invalid minimum value for remote id and circuit id
	 * using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)</li>
	 * <li>Verify setting the invalid Maximum value for remote id and circuit id
	 * using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)</li>
	 * <li>Verify the WebPA GET command to obtain the Wi-Fi Mode on 2.4GHz is
	 * executed successfully</li>
	 * <li>Verify retrieving the configuration of Wifi Mode on 2.4 Ghz using SNMP
	 * OID ( 1.3.6.1.4.1.17270.50.2.2.7.1.1.1.10000)</li>
	 * <li>Verify the WebPA GET command to obtain the Wi-Fi Mode on 5GHz is executed
	 * successfully</li>
	 * <li>Verify retrieving the configuration of Wifi Mode on 5Ghz using SNMP OID
	 * (1.3.6.1.4.1.17270.50.2.2.7.1.1.1.10100 )</li> *
	 * <li>Verify retrieving the configuration of WAN MAC address using WEBPA
	 * parameter(Device.DeviceInfo.X_COMCAST-COM_WAN_MAC)</li>
	 * <li>Verify retrieving the configuration of WAN MAC address using SNMP OID
	 * (.1.3.6.1.2.1.2.2.1.6.1 )</li>
	 * <li>Verify the WebPA GET command to obtain the configuration of bridge mode
	 * is executed successfully</li>
	 * <li>Verify retrieving the configuration of bridge mode status using SNMP OID
	 * (1.3.6.1.4.1.17270.50.2.3.2.1.1.32)</li>
	 * <li>Verify retrieving the configuration of hotspotEnable using SNMP OID
	 * (1.3.6.1.4.1.17270.50.2.13.1.1.0)</li>
	 * <li>Verify retrieving the configuration of bssid MAC for 2.4Ghz using
	 * webpa</li>
	 * <li>Verify retrieving the configuration of bssid MAC for 5Ghz webpa</li>
	 * </ol>
	 * 
     * @param device
     *            {@link Dut}
     * 
     * @author Joseph M
     * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WiFi-SNMP-1031")
	public void verifyConfigurationUsingSnmp(Dut device) {
		// Variable Declaration begins
		String testCaseId = "";
		String stepNum = "";
		String errorMessage = "";
		String response = null; // String to store snmp response
		boolean status = false; // boolean varibale to store the status.
		String wanMac = ""; // Variable to store the WAN MAC address.
		String formatedWanMac = "";// Variable to store wan mac without leading zeros.
		String wifiMode2_4GHz = null;// String variable to store Wi-Fi mode for 2.4GHz.
		String wifiMode5GHz = null;// String variable to store Wi-Fi mode for 5GHz.
		String wanMacAddressIndex = null;// String variable to store SNMP oid index for Wan mac address
		boolean isInvalidChannelSetFor2_4Ghz = false;
		boolean isInvalidChannelSetFor5Ghz = false;
		// Variable Declation Ends

		testCaseId = "TC-RDKB-WiFi-SNMP-031";

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-Wi-Fi-SNMP-1031");
		LOGGER.info("TEST DESCRIPTION: Verify SNMP configuration of Wifi");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Verify  retrieving the configuration of channel selection for 2.4Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000)");
		LOGGER.info(
				"2.Verify  setting the invalid   channel selection value as 12 for 2.4Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000)");
		LOGGER.info(
				"3. Verify  retrieving the configuration of channel selection for 5Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100)");
		LOGGER.info(
				"4. Verify  setting the Invalid   channel selection value as 38 for 5Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100)");
		LOGGER.info("5. Verify  retrieving the configuration of Max Concurrent Client Limit on 2.4 Ghz  using "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
		LOGGER.info("6. Verify  setting  the minimum value for  Max Concurrent Client Limit on 2.4 Ghz  using "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
		LOGGER.info("7. Verify  setting  the Maximum value for  Max Concurrent Client Limit on 2.4 Ghz  using "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
		LOGGER.info("8. Verify  setting  the invalid minimum value for  Max Concurrent Client Limit on 2.4 Ghz  "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
		LOGGER.info("9. Verify  setting  the invalid maximum value for  Max Concurrent Client Limit on 2.4 Ghz "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
		LOGGER.info("10. Verify  retrieving the configuration of Max Concurrent Client Limit on  5Ghz  using "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
		LOGGER.info("11. Verify  setting  the minimum value for  Max Concurrent Client Limit on 5Ghz  using"
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
		LOGGER.info("12. Verify  setting  the Maximum value for  Max Concurrent Client Limit on 5Ghz  using "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
		LOGGER.info("13. Verify  setting  the invalid minimum value for  Max Concurrent Client Limit on 5Ghz  "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
		LOGGER.info("14. Verify  setting  the invalid maximum value for  Max Concurrent Client Limit on 5Ghz  "
				+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
		LOGGER.info(
				"15.Verify  retrieving the configuration of remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
		LOGGER.info(
				"16.Verify  setting  the minimum value for  remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
		LOGGER.info(
				"17.Verify  setting  the Maximum value for  remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
		LOGGER.info(
				"18.Verify  setting  the invalid minimum value for  remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
		LOGGER.info(
				"19.Verify  setting  the invalid maximum  value for  remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
		LOGGER.info("20. Verify the WebPA GET command to obtain the Wi-Fi Mode on 2.4 GHz is executed successfully");
		LOGGER.info(
				"21. Verify  retrieving the configuration of Wifi Mode on 2.4 Ghz  using SNMP OID ( 1.3.6.1.4.1.17270.50.2.2.7.1.1.1.10000)");
		LOGGER.info("22. Verify the WebPA GET command to obtain the Wi-Fi Mode on 5GHz is executed successfully");
		LOGGER.info(
				"23. Verify  retrieving the configuration of Wifi Mode on 5Ghz  using SNMP OID (1.3.6.1.4.1.17270.50.2.2.7.1.1.1.10100 )");
		LOGGER.info(
				"24. Verify  retrieving the configuration of WAN MAC address  using WEBPA parameter(Device.DeviceInfo.X_COMCAST-COM_WAN_MAC).");
		LOGGER.info(
				"25. Verify  retrieving the configuration of WAN MAC address  using SNMP OID (.1.3.6.1.2.1.2.2.1.6.1 )");
		LOGGER.info(
				"26. Verify the WebPA GET command to obtain the configuration of bridge mode is executed successfully");
		LOGGER.info(
				"27. Verify retrieving the configuration of bridge mode status using SNMP OID (1.3.6.1.4.1.17270.50.2.3.2.1.1.32)");
		LOGGER.info(
				"28. Verify retrieving the configuration of hotspotEnable using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.1.0)");
		LOGGER.info("29. Verify retrieving the configuration of bssid MAC for 2.4Ghz using webpa");
		LOGGER.info("30. Verify retrieving the configuration of bssid MAC for 5Ghz using webpa");
		LOGGER.info("#######################################################################################");

		try {
			LOGGER.info("################################# STARTING PRE-CONFIGURATIONS #############################");
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION 1: DESCRIPTION: Factory reset the device to check default values");
			LOGGER.info("PRE-CONDITION 1: EXPECTED: Factory resetting the device should be successful");
			if (!BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device)) {
				LOGGER.info("PRE-CONDITION 1 FAILED: FACTORY RESETTING THE DEVICE FAILED");
				throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + "Factory Resetting the device failed");
			}
			LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

			LOGGER.info("Waiting for Three minutes for changes to apply.");
			tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);

			stepNum = "S1";
			errorMessage = "Unable to Retrieve the of Channel Selection for 2.4Ghz using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify  retrieving the configuration of channel selection for 2.4Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000)");
			LOGGER.info(
					"STEP 1: ACTION : Execute the SNMP command to retrieve Channel Selection for 2.4Ghzsnmpget -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000");
			LOGGER.info(
					"STEP 1: EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.2.6.1.1.3.10000 = 0");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_2_4_GHZ.getOid(),
					BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_2_4_GHZ.getTableIndex());
			LOGGER.info("Channel selection for 2.4Ghz retrieved using SNMP = " + response);
			errorMessage = BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response,
					BroadBandTestConstants.STRING_ZERO);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : The Channel Selection for 2.4Ghz using SNMP was successful retrieved.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S2";
			errorMessage = "Able to set  the invalid  channel selection value as '12' for 2.4Ghz using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify  setting the invalid   channel selection value as 12 for 2.4Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000)");
			LOGGER.info(
					"STEP 2: ACTION : Execute the SNMP command to set the  invalid  Channel Selection value for 2.4Ghz snmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000 u 12");
			LOGGER.info("STEP 2: EXPECTED : should not be able to set the invalid value as 12 for 2.4Ghz using SNMP");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_2_4_GHZ.getOid(), SnmpDataType.UNSIGNED_INTEGER,
					BroadBandTestConstants.STRING_VALUE_12,
					BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_2_4_GHZ.getTableIndex());
			LOGGER.info("Invalid  Channel selection value for 2.4Ghz retrieved using SNMP = " + response);
			if (!CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)) {
				status = CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.WRONG_VALUE)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.GENERAL_FAILURE_VALUE);
			} else {
				errorMessage = errorMessage + BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			}
			LOGGER.info("Setting the Invalid  Channel selection value for 2.4Ghz retrieved using SNMP = " + response);
			if (status) {
				isInvalidChannelSetFor2_4Ghz = status;
				LOGGER.info(
						"STEP 2: ACTUAL : Setting the invalid  channel selection value as '12' for 2.4Ghz using SNMP failed.");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S3";
			errorMessage = "Unable to Retrieve the Channel Selection for 5Ghz using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Verify  retrieving the configuration of Channel Selection for 5Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100)");
			LOGGER.info(
					"STEP 3: ACTION : Execute the SNMP command to retrieve Channel Selection for 5Ghz snmpget -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100");
			LOGGER.info(
					"STEP 3: EXPECTED : Expected output should be as,SNMPv2-SMI::enterprises.17270.50.2.2.6.1.1.3.10100 = 0");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_5_GHZ.getOid(),
					BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_5_GHZ.getTableIndex());
			LOGGER.info("Channel selection for 5Ghz retrieved using SNMP = " + response);
			errorMessage = BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response,
					BroadBandTestConstants.STRING_ZERO);
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : The Channel Selection for 5Ghz was retrieved successful");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S4";
			errorMessage = "Able to set  the Invalid  channel selection value as '50' for 5Ghz using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify  setting the Invalid   channel selection value as 50 for 5Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100)");
			LOGGER.info(
					"STEP 4: ACTION : Execute the SNMP command to set the  Invalid  Channel Selection value for 5Ghz snmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100 u 50");
			LOGGER.info("STEP 4: EXPECTED : should not be able to set the Invalid value as 50 for 5Ghz using SNMP");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_5_GHZ.getOid(), SnmpDataType.UNSIGNED_INTEGER,
					BroadBandTestConstants.STRING_VALUE_50,
					BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_5_GHZ.getTableIndex());
			LOGGER.info("Invalid maximum Channel selection value for 5Ghz retrieved using SNMP = " + response);
			if (!CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)) {
				status = CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.WRONG_VALUE)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.GENERAL_FAILURE_VALUE);
			} else {
				errorMessage = errorMessage + BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			}
			LOGGER.info("Setting the Invalid  Channel selection value for 5Ghz retrieved using SNMP = " + response);
			if (status) {
				isInvalidChannelSetFor5Ghz = status;
				LOGGER.info(
						"STEP 4: ACTUAL : Setting the invalid   channel selection value for 5Ghz using SNMP failed.");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S5";
			errorMessage = "Unable to Retrieve the Max Concurrent Client Limit on 2.4 Ghz using webpa";
			status = false;
			String expectedValue = BroadBandTestConstants.STRING_VALUE_FIVE;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Verify  retrieving the configuration of Max Concurrent Client Limit on 2.4 Ghz  using "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
			LOGGER.info("STEP 5: ACTION : Execute the webpa command to retrieve Max Concurrent Client Limit on 2.4Ghz "
					+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
			LOGGER.info(
					"STEP 5: EXPECTED : Expected output should be as 15 for Business devices and for other models it should be 5");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4,
					DeviceModeHandler.isBusinessClassDevice(device) ? BroadBandTestConstants.STRING_VALUE_FIFTEEN
							: expectedValue);

			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL :The Max Concurrent Client Limit on 2.4 Ghz was retrieved successfully using webpa.");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S6";
			errorMessage = "Unable to set  the minimum value for Max Concurrent Client Limit  as '1' on 2.4 Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify  setting  the minimum value for  Max Concurrent Client Limit on 2.4 Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
			LOGGER.info(
					"STEP 6: ACTION : Execute the webpa command to set the minimum value for Max Concurrent Client Limit on 2.4Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
			LOGGER.info("STEP 6: EXPECTED : Expected output should be as,  1");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4,
					BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_VALUE_ONE);

			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL :setting  the minimum value for Max Concurrent Client Limit on 2.4 Ghz was  successfully using webpa.");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S7";
			errorMessage = "Unable to set  the Maximum value for Max Concurrent Client Limit  as '128' on 2.4 Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 7: DESCRIPTION : Verify  setting  the Maximum value for  Max Concurrent Client Limit on 2.4 Ghz  using "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);

			LOGGER.info(
					"STEP 7: ACTION : Execute the SNMP command to set the Maximum value for Max Concurrent Client Limit on 2.4Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);

			LOGGER.info("STEP 7: EXPECTED : Expected output should be as, 128");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4,
					BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_VALUE_ONE_HUNDRED_AND_TWENTY_EIGHT);

			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL :setting  the Maximum value for Max Concurrent Client Limit on 2.4 Ghz was  successfully using webpa.");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S8";
			errorMessage = "unable to set  the  minimum value for Max Concurrent Client Limit  as '0' on 2.4 Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8: DESCRIPTION : Verify  setting  the  minimum value for  Max Concurrent Client Limit on 2.4 Ghz  "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);

			LOGGER.info(
					"STEP 8: ACTION : Execute the webpa command to set the  minimum value for Max Concurrent Client Limit on 2.4Ghz"
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);

			LOGGER.info(
					"STEP 8: EXPECTED : should  be able to set the Invalid minimum value for Max Concurrent Client Limit  as '0' using webpa");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4,
					BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_ZERO);

			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL :setting  the  minimum value for Max Concurrent Client Limit on 2.4 Ghz was  failed using webpa.");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S9";
			errorMessage = "unable to set  the  maximum value for Max Concurrent Client Limit  as '148' on 2.4 Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Verify  setting  the  maximum value for  Max Concurrent Client Limit on 2.4 Ghz  "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
			LOGGER.info(
					"STEP 9: ACTION : Execute the webpa command to set the  maximum value for Max Concurrent Client Limit on 2.4Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
			LOGGER.info(
					"STEP 9: EXPECTED : should  be able to set the  maximum value for Max Concurrent Client Limit  as '148' using webpa");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4,
					BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_VALUE_ONE_HUNDRED_AND_FORTY_EIGHT);

			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL :setting  the  maximum value for Max Concurrent Client Limit on 2.4 Ghz was  failed using webpa.");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S10";
			errorMessage = "Unable to Retrieve Max Concurrent Client Limit on 5Ghz using SNMP";
			status = false;
			expectedValue = BroadBandTestConstants.STRING_VALUE_FIVE;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 10: DESCRIPTION : Verify  retrieving the configuration of Max Concurrent Client Limit on  5Ghz  ");
			LOGGER.info("STEP 10: ACTION : Execute the webpa command to retrieve Max Concurrent Client Limit on  5Ghz "
					+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
			LOGGER.info("STEP 10: EXPECTED : Expected output should be as,  5");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5,
					DeviceModeHandler.isBusinessClassDevice(device) ? BroadBandTestConstants.STRING_VALUE_FIFTEEN
							: expectedValue);

			LOGGER.info("Configuration of Max Concurrent Client Limit on 5Ghz retrieved using SNMP = " + response);
			if (DeviceModeHandler.isBusinessClassDevice(device)) {
				expectedValue = BroadBandTestConstants.STRING_VALUE_FIFTEEN;
			}
			errorMessage = BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response, expectedValue);
			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL :The Max Concurrent Client Limit on 5Ghz using webpa is retrieved Successful");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S11";
			errorMessage = "Unable to set  the minimum value for Max Concurrent Client Limit  as '1' on  5Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 11: DESCRIPTION : Verify  setting  the minimum value for  Max Concurrent Client Limit on  5Ghz  ");
			LOGGER.info(
					"STEP 11: ACTION : Execute the webpa command to set the minimum value for Max Concurrent Client Limit on  5Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
			LOGGER.info("STEP 11: EXPECTED : Expected output should be as,  1");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5,
					BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_VALUE_ONE);
			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL :setting  the minimum value for Max Concurrent Client Limit on  5Ghz was  successfully using webpa.");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S12";
			errorMessage = "Unable to set  the Maximum value for Max Concurrent Client Limit  as '128' on  5Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 12: DESCRIPTION : Verify  setting  the Maximum value for  Max Concurrent Client Limit on  5Ghz ");
			LOGGER.info(
					"STEP 12: ACTION : Execute the webpa command to set the Maximum value for Max Concurrent Client Limit on 5Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
			LOGGER.info("STEP 12: EXPECTED : Expected output should be as,  128");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5,
					BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_VALUE_ONE_HUNDRED_AND_TWENTY_EIGHT);
			if (status) {
				LOGGER.info(
						"STEP 12: ACTUAL :setting  the Maximum value for Max Concurrent Client Limit on 5Ghz was  successfully using webpa.");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S13";
			errorMessage = "unable to set  the  minimum value for Max Concurrent Client Limit  as '0' on 5Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 13: DESCRIPTION : Verify  setting  the  minimum value for  Max Concurrent Client Limit on 5Ghz  ");
			LOGGER.info(
					"STEP 13: ACTION : Execute the webpa command to set the  minimum value for Max Concurrent Client Limit on 5Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
			LOGGER.info(
					"STEP 13: EXPECTED : should  be able to set the  minimum value for Max Concurrent Client Limit  as '0' using webpa");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5,
					BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_ZERO);
			if (status) {
				LOGGER.info(
						"STEP 13: ACTUAL :setting  the  minimum value for Max Concurrent Client Limit on 5Ghz was  failed using webpa.");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S14";
			errorMessage = "unable to set  the  maximum value for Max Concurrent Client Limit  as '148' on 5Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 14: DESCRIPTION : Verify  setting  the  maximum value for  Max Concurrent Client Limit on 5Ghz ");
			LOGGER.info(
					"STEP 14: ACTION : Execute the webpa command to set the  maximum value for Max Concurrent Client Limit on 5Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
			LOGGER.info(
					"STEP 14: EXPECTED : should be able to set the  maximum value for Max Concurrent Client Limit  as '148' using webpa");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5,
					BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_VALUE_ONE_HUNDRED_AND_FORTY_EIGHT);
			if (status) {
				LOGGER.info(
						"STEP 14: ACTUAL :setting  the  maximum value for Max Concurrent Client Limit on 5Ghz was  failed using webpa.");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S15";
			errorMessage = "Unable to retrieve the remote id and circuit id  using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 15: DESCRIPTION : Verify  retrieving the configuration of remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
			LOGGER.info(
					"STEP 15: ACTION : Execute the SNMP command to retrieve remote id and circuit id snmpget -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3");
			LOGGER.info(
					"STEP 15: EXPECTED : Expected output should be as below,SNMPv2-SMI::enterprises.17270.50.2.13.1.2.1.11.3 = Gauge32: 3");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getOid(),
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getTableIndex());
			LOGGER.info("Remote id and Circuit id retrieved using SNMP = " + response);
			errorMessage = BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response,
					BroadBandTestConstants.STRING_VALUE_THREE);
			if (status) {
				LOGGER.info("STEP 15: ACTUAL : The remote id and Circuit id using SNMP is retrieved Successful");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S16";
			errorMessage = "Unable to set  the minimum value for  remote id and circuit id as '0'  using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 16: DESCRIPTION : Verify  setting  the minimum value for  remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
			LOGGER.info(
					"STEP 16: ACTION : Execute the SNMP command to set the minimum value for remote id and circuit id snmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3 u 0");
			LOGGER.info(
					"STEP 16: EXPECTED : Expected output should be as below,SNMPv2-SMI::enterprises.17270.50.2.13.1.2.1.11.3 = 0");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getOid(), SnmpDataType.UNSIGNED_INTEGER,
					BroadBandTestConstants.STRING_ZERO, BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getTableIndex());
			LOGGER.info("setting  the minimum value for Remote id and Circuit id  using SNMP = " + response);

			errorMessage = BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response,
					BroadBandTestConstants.STRING_ZERO);
			if (status) {
				LOGGER.info(
						"STEP 16: ACTUAL : setting  the minimum value for  remote id and circuit id  using SNMP is  Successful");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S17";
			errorMessage = "Unable to set  the Maximum value for  remote id and circuit id as '7'  using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 17: DESCRIPTION : Verify  setting  the Maximum value for  remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
			LOGGER.info(
					"STEP 17: ACTION : Execute the SNMP command to set the Maximum value for remote id and circuit id snmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3 u 7");
			LOGGER.info(
					"STEP 17: EXPECTED : Expected output should be as below,SNMPv2-SMI::enterprises.17270.50.2.13.1.2.1.11.3 = 7");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getOid(), SnmpDataType.UNSIGNED_INTEGER,
					BroadBandTestConstants.STRING_VALUE_SEVEN,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getTableIndex());
			LOGGER.info("Setting  the Maximum value for Remote id and Circuit id  using SNMP = " + response);
			errorMessage = BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response,
					BroadBandTestConstants.STRING_VALUE_SEVEN);
			if (status) {
				LOGGER.info(
						"STEP 17: ACTUAL : setting  the Maximum value for  remote id and circuit id  using SNMP is  Successful");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S18";
			errorMessage = "able to set  the invalid minimum value for  remote id and circuit id as '-1'  using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 18: DESCRIPTION : Verify  setting  the invalid minimum value for  remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
			LOGGER.info(
					"STEP 18: ACTION : Execute the SNMP command to set the invalid minimum value for remote id and circuit id snmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3 u -1");
			LOGGER.info("STEP 18: EXPECTED : Should not be able to set the Invalid minimum value as -1");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getOid(), SnmpDataType.UNSIGNED_INTEGER,
					BroadBandTestConstants.STRING_CONSTANT_NEGATIVE_1,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getTableIndex());
			LOGGER.info("setting  the invalid minimum value for Remote id and Circuit id  using SNMP = " + response);
			if (!CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)) {
				status = CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.WRONG_VALUE)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.GENERAL_FAILURE_VALUE)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.BAD_FAILURE_VALUE);
			} else {
				errorMessage = errorMessage + BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			}
			if (status) {
				LOGGER.info(
						"STEP 18: ACTUAL : setting  the invalid minimum value for  remote id and circuit id  using SNMP failed");
			} else {
				LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S19";
			errorMessage = "Able to set  the invalid maximum value for  remote id and circuit id as '8'  using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 19: DESCRIPTION : Verify  setting  the invalid maximum value for  remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");
			LOGGER.info(
					"STEP 19: ACTION : Execute the SNMP command to set the invalid maximum value for remote id and circuit id snmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3 u 8");
			LOGGER.info("STEP 19: EXPECTED : Should not be able to set the Invalid maximum value as 8");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getOid(), SnmpDataType.UNSIGNED_INTEGER,
					BroadBandTestConstants.STRING_VALUE_EIGHT,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getTableIndex());
			LOGGER.info("Setting  the Invalid maximum value for Remote id and Circuit id  using SNMP = " + response);
			if (!CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)) {
				status = CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.WRONG_VALUE)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.GENERAL_FAILURE_VALUE)
						|| CommonUtils.patternSearchFromTargetString(response,
								BroadBandTestConstants.BAD_FAILURE_VALUE);
			} else {
				errorMessage = errorMessage + BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			}
			if (status) {
				LOGGER.info(
						"STEP 19: ACTUAL : setting  the invalid maximum value for  remote id and circuit id  using SNMP failed");
			} else {
				LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S20";
			errorMessage = "Unable to retrieve the status of Wi-Fi Mode on 2.4 GHz'Device.WiFi.Radio.1.OperatingStandards' using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 20: DESCRIPTION: Verify the WebPA GET command to obtain the Wi-Fi Mode on 2.4GHz is executed successfully");
			LOGGER.info(
					"STEP 20: ACTION: Execute the TR-181 parameter-Device.WiFi.Radio.10000.OperatingStandards to retrieve Wi-Fi Mode on 2.4GHz by WebPA");
			LOGGER.info("STEP 20: EXPECTED: WEBPA request should respond with value as 'g,n' and status code 200");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to retrieve the status of Wi-Fi Mode on 2.4 GHz'Device.WiFi.Radio.10000.OperatingStandards' using WebPA command.";
			wifiMode2_4GHz = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_WIFI_MODE_ON_2_4);
			LOGGER.info("Wi-Fi Mode on 2.4 GHz retrieved using WebPa= " + wifiMode2_4GHz);
			status = CommonMethods.isNotNull(wifiMode2_4GHz);
			if (status) {
				LOGGER.info(
						"S20 ACTUAL: Successfully retrieved  the status of Wi-Fi Mode on 2.4GHz 'Device.WiFi.Radio.2.OperatingStandards' using WebPA command.");
			} else {
				LOGGER.error("S20 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S21";
			errorMessage = "Unable to retrieve the Wifi Mode on 2.4Ghz using SNMP and cross verfication with WEBPA parameter was not successful.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 21: DESCRIPTION : Verify  retrieving the configuration of Wifi Mode on 2.4Ghz  using SNMP OID ( 1.3.6.1.4.1.17270.50.2.2.7.1.1.1.10000)");
			LOGGER.info(
					"STEP 21: ACTION : Execute the SNMP command to retrieve Wifi Mode on 2.4Ghz snmpget -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.2.7.1.1.1.10000");
			LOGGER.info(
					"STEP 21: EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.2.7.1.1.1.10000 = INTEGER: 160");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_WIFIMODE_2_4_GHZ.getOid(),
					BroadBandSnmpMib.ECM_CONFIG_WIFIMODE_2_4_GHZ.getTableIndex());
			LOGGER.info("Wifi Mode on 2.4Ghz retrieved using SNMP = " + response);
			if (!CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)) {
				status = CommonMethods.isNotNull(response) && response
						.equalsIgnoreCase(BroadBandTestConstants.WIFI_MODE_VALUE_FROM_SNMP_MAPPING.get(wifiMode2_4GHz));
			} else {
				errorMessage = errorMessage + BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			}
			if (status) {
				LOGGER.info(
						"STEP 21: ACTUAL : The Wifi Mode on 2.4Ghz using SNMP is retrieved and cross verification with WEBPA value was successful.");
			} else {
				LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S22";
			errorMessage = "Unable to retrieve the status of Wi-Fi Mode on 5GHz 'Device.WiFi.Radio.2.OperatingStandards' using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 22: DESCRIPTION: Verify the WebPA GET command to obtain the Wi-Fi Mode on 5GHz is executed successfully");
			LOGGER.info(
					"STEP 22: ACTION: Execute the TR-181 parameter-Device.WiFi.Radio.10100.OperatingStandards to retrieve Wi-Fi Mode on 5GHz by WebPA");
			LOGGER.info("STEP 22: EXPECTED: WEBPA request should respond with value as 'a,n,ac' and status code 200");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to retrieve the status of Wi-Fi Mode on 5GHz'Device.WiFi.Radio.10100.OperatingStandards' using WebPA command.";
			wifiMode5GHz = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_WIFI_MODE_ON_5);
			LOGGER.info("Wi-Fi Mode on 5GHz retrieved using WebPa = " + wifiMode5GHz);
			status = CommonMethods.isNotNull(wifiMode5GHz);
			if (status) {
				LOGGER.info(
						"S22 ACTUAL: Successfully retrieved  the status of Wi-Fi Mode on 5GHz 'Device.WiFi.Radio.10100.OperatingStandards' using WebPA command.");
			} else {
				LOGGER.error("S22 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S23";
			errorMessage = "Unable to retrieve the Wifi Mode on 5 Ghz using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 23: DESCRIPTION : Verify  retrieving the configuration of Wifi Mode on 5Ghz  using SNMP OID (1.3.6.1.4.1.17270.50.2.2.7.1.1.1.10100 )");
			LOGGER.info(
					"STEP 23: ACTION : Execute the SNMP command to retrieve Wifi Mode on 5Ghz snmpget -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.2.7.1.1.1.10100");
			LOGGER.info(
					"STEP 23: EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.2.7.1.1.1.10100 = INTEGER: 448");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_WIFIMODE_5_GHZ.getOid(),
					BroadBandSnmpMib.ECM_CONFIG_WIFIMODE_5_GHZ.getTableIndex());
			LOGGER.info("Wifi Mode on 5Ghz retrieved using SNMP = " + response);
			if (!CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)) {
				status = CommonMethods.isNotNull(response) && response
						.equalsIgnoreCase(BroadBandTestConstants.WIFI_MODE_VALUE_FROM_SNMP_MAPPING.get(wifiMode5GHz));
			} else {
				errorMessage = errorMessage + BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			}
			if (status) {
				LOGGER.info(
						"STEP 23: ACTUAL : The Wifi Mode on 5Ghz using SNMP is retrieved and cross verification with WEBPA value was successfully.");
			} else {
				LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S24";
			errorMessage = "Unable to retrieve the WAN MAC address using WEBPA Parameter(Device.DeviceInfo.X_COMCAST-COM_WAN_MAC)";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 24: DESCRIPTION : Verify  retrieving the configuration of WAN MAC address using WEBPA parameter(Device.DeviceInfo.X_COMCAST-COM_WAN_MAC).");
			LOGGER.info(
					"STEP 24: ACTION : Execute the WEBPA Command  with parameter Device.DeviceInfo.X_COMCAST-COM_WAN_MAC");
			LOGGER.info(
					"STEP 24: EXPECTED : Expected output should be as below,IF-MIB::ifPhysAddress.1 = STRING: 10:56:11:a2:17:40");
			LOGGER.info("**********************************************************************************");
			wanMac = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WAN_MAC_ADDRESS);
			formatedWanMac = BroadBandCommonUtils.formatIpOrMacWithoutLeadingZeros(wanMac);
			LOGGER.info("WAN MAC address retrieved using WebPa = " + wanMac);
			status = CommonMethods.isNotNull(wanMac) && CommonMethods.patternMatcher(formatedWanMac.trim(),
					BroadBandTestConstants.PATTERN_TO_VALIDATE_MAC_ADDRESS);
			if (status) {
				LOGGER.info(
						"STEP 24: ACTUAL : WAN MAC address is retrieved successfully using the WEBPA parameter(Device.DeviceInfo.X_COMCAST-COM_WAN_MAC).");
			} else {
				LOGGER.error("STEP 24: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S25";
			errorMessage = "Unable to retrieve the WAN MAC address using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 25: DESCRIPTION : Verify  retrieving the configuration of WAN MAC address using SNMP OID (.1.3.6.1.2.1.2.2.1.6.1 )");
			LOGGER.info(
					"STEP 25: ACTION : Execute the SNMP command to retrieve WAN MAC address snmpget -v2c -c community_string udp6:<CMIP> .1.3.6.1.2.1.2.2.1.6.1");
			LOGGER.info(
					"STEP 25: EXPECTED : Expected output should be as, IF-MIB::ifPhysAddress.1 = STRING: 10:56:11:a2:17:40");
			LOGGER.info("**********************************************************************************");
			if (DeviceModeHandler.isFibreDevice(device)) {
				wanMacAddressIndex = BroadBandTestConstants.STRING_VALUE_TWENTY_FIVE;
			} else {
				wanMacAddressIndex = BroadBandSnmpMib.ECM_CONFIG_WANMAC_ADDRESS.getTableIndex();
			}
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_WANMAC_ADDRESS.getOid(), wanMacAddressIndex);
			LOGGER.info("WAN MAC address retrieved using SNMP after trimming = " + response.trim());
			LOGGER.info("WAN MAC address retrieved using WEBPA after trimming = " + formatedWanMac.trim());
			status = CommonMethods.isNotNull(response) && response.trim().equalsIgnoreCase(formatedWanMac.trim());
			if (status) {
				LOGGER.info("STEP 25: ACTUAL : WAN MAC address is retrieved successfully using SNMP get command.");
			} else {
				LOGGER.error("STEP 25: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S26";
			errorMessage = "Unable to retrieve the configuration of Bridge Mode status'Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode' using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 26: DESCRIPTION: Verify the WebPA GET command to obtain the configuration of Bridge Mode is executed successfully");
			LOGGER.info(
					"STEP 26: ACTION: Execute the TR-181 parameter-'Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode' to retrieve configuration of bridge mode by WebPA");
			LOGGER.info("STEP 26: EXPECTED: WEBPA request should respond with value as 'router' and status code 200");
			LOGGER.info("**********************************************************************************");
			String bridgeModeStatus = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_BRIDGE_MODE_STATUS);
			LOGGER.info("Bridge mode status retrieved using WebPa = " + bridgeModeStatus);
			status = CommonMethods.isNotNull(bridgeModeStatus);
			if (status) {
				LOGGER.info(
						"S26 ACTUAL: Successfully retrieved  the status of Bridge Mode 'Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode' using WebPA command.");
			} else {
				LOGGER.error("S26 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S27";
			errorMessage = "Unable to retrieve the configuration of Bridge Mode status using SNMP";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 27: DESCRIPTION : Verify retrieving the configuration of bridge mode status using SNMP OID (1.3.6.1.4.1.17270.50.2.3.2.1.1.32)");
			LOGGER.info(
					"STEP 27: ACTION : Execute the SNMP command to retrieve bridge mode status snmpget -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.3.2.1.1.32");
			LOGGER.info(
					"STEP 27: EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.3.2.1.1.32 = INTEGER: 2");
			LOGGER.info("**********************************************************************************");
			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_BRIDGEMODE_STATUS.getOid(),
					BroadBandSnmpMib.ECM_CONFIG_BRIDGEMODE_STATUS.getTableIndex());
			LOGGER.info("Bridge mode status retrieved using SNMP = " + response);
			errorMessage = errorMessage + BroadBandTestConstants.SNMPV3_EXECUTION_ERROR;
			status = BroadBandSnmpUtils.validateSNMPResponse(tapEnv, device, response,
					BroadBandTestConstants.BRIDGE_MODE_VALUE_FROM_SNMP_MAPPING.get(bridgeModeStatus));
			if (status) {
				LOGGER.info("STEP 27: ACTUAL : The Bridge Mode status using SNMP is retrieved Successfully");
			} else {
				LOGGER.error("STEP 27: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S28";
			errorMessage = "Unable to retrieve the hotspotEnable using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 28: DESCRIPTION : Verify retrieving the configuration of hotspotEnable");
			LOGGER.info("STEP 28: ACTION : Execute the webpa command to the configuration of hotspotEnable "
					+ BroadBandWebPaConstants.WEBPA_PARAM_PUBLIC_WIFI_ENABLE);
			LOGGER.info("STEP 28: EXPECTED : Expected output should be as false");
			LOGGER.info("**********************************************************************************");
			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_PUBLIC_WIFI_ENABLE);
			status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP 28: ACTUAL : The hotspotEnable using webpa is retrieved successfully");
			} else {
				LOGGER.error("STEP 28: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S29";
			errorMessage = "Unable to retrieve the bssid MAC for 2.4Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 29: DESCRIPTION : Verify retrieving the configuration of BSSID MAC for 2.4Ghz");
			LOGGER.info(
					"STEP 29: ACTION : Execute the webpa command to retrieve the configuration of BSSID MAC for 2.4Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PUBLIC_WIFI_SSID_BSSID.replace(
									BroadBandTestConstants.TR181_NODE_REF, WEBPA_AP_INDEXES.PUBLIC_WIFI.get24Ghz()));
			LOGGER.info("STEP 29: EXPECTED : Expected output should be as  B4 2A 0E 46 59 4B");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PUBLIC_WIFI_SSID_BSSID
							.replace(BroadBandTestConstants.TR181_NODE_REF, WEBPA_AP_INDEXES.PUBLIC_WIFI.get24Ghz()));
			LOGGER.info("2.4 GHz Public wifi BSSID Mac = " + response);
			status = CommonMethods.isNotNull(response) && CommonMethods.isMacValid(response);

			if (status) {
				LOGGER.info("STEP 29: ACTUAL : The BSSID MAC for 2.4Ghz using webpa is retrieved successfully");
			} else {
				LOGGER.error("STEP 29: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S30";
			errorMessage = "Unable to retrieve the bssid MAC for 5Ghz using webpa";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 30: DESCRIPTION : Verify retrieving the configuration of BSSID MAC for 5Ghz ");
			LOGGER.info(
					"STEP 30: ACTION : Execute the snmp command to retrieve the configuration of BSSID MAC for 5Ghz "
							+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PUBLIC_WIFI_SSID_BSSID.replace(
									BroadBandTestConstants.TR181_NODE_REF, WEBPA_AP_INDEXES.PUBLIC_WIFI.get5Ghz()));
			LOGGER.info("STEP 30: EXPECTED : Expected output should be as below, B4 2A 0E 46 59 4B");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PUBLIC_WIFI_SSID_BSSID
							.replace(BroadBandTestConstants.TR181_NODE_REF, WEBPA_AP_INDEXES.PUBLIC_WIFI.get5Ghz()));
			LOGGER.info("5 GHz Public wifi BSSID Mac = " + response);
			status = CommonMethods.isNotNull(response) && CommonMethods.isMacValid(response);
			if (status) {
				LOGGER.info("STEP 30: ACTUAL : The BSSID MAC for 5Ghz using webpa is retrieved successfully");
			} else {
				LOGGER.error("STEP 30: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.info(errorMessage);
		} finally {
			LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
			LOGGER.info("################# Reactivating the Device after successful factory reset #################");
			BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
			int step = 1;
			if (!isInvalidChannelSetFor2_4Ghz) {
				LOGGER.info("POST-CONDITION " + step
						+ ": DESCRIPTION: Verify  setting  the default  configuration of channel selection for 2.4Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000)");

				LOGGER.info("POST-CONDITION " + step
						+ ": ACTION :  Execute the SNMP command to set the default Channel Selection for 2.4Ghzsnmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10000 u 0");
				LOGGER.info("POST-CONDITION " + step
						+ ": EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.2.6.1.1.3.10000 = 0");

				response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_2_4_GHZ.getOid(), SnmpDataType.UNSIGNED_INTEGER,
						BroadBandTestConstants.STRING_ZERO,
						BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_2_4_GHZ.getTableIndex());
				status = CommonMethods.isNotNull(response)
						&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
				if (status) {
					LOGGER.info("POST-CONDITION " + step
							+ " ACTUAL: The default  Channel Selection  value for 2.4Ghz using SNMP was set successfully.");
				} else {
					LOGGER.error("POST-CONDITION " + step
							+ " ACTUAL: Unable to set the default  Channel Selection  value for 2.4Ghz using SNMP");
				}
				step++; // NEXT POST-CONDITION
			}

			if (!isInvalidChannelSetFor5Ghz) {
				LOGGER.info("POST-CONDITION " + step
						+ ": DESCRIPTION: Verify  setting  the default  configuration of channel selection for 5Ghz using SNMP OID (1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100)");

				LOGGER.info("POST-CONDITION " + step
						+ ": ACTION :  Execute the SNMP command to set the default Channel Selection for 5Ghzsnmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.2.6.1.1.3.10100 u 0");
				LOGGER.info("POST-CONDITION " + step
						+ ": EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.2.6.1.1.3.10100 = 0");

				response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_5_GHZ.getOid(), SnmpDataType.UNSIGNED_INTEGER,
						BroadBandTestConstants.STRING_ZERO,
						BroadBandSnmpMib.ECM_CONFIG_CHANNELSELECTION_5_GHZ.getTableIndex());
				status = CommonMethods.isNotNull(response)
						&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
				if (status) {
					LOGGER.info("POST-CONDITION " + step
							+ " ACTUAL: The default  Channel Selection  value for 5Ghz using SNMP was set successfully.");
				} else {
					LOGGER.error("POST-CONDITION " + step
							+ " ACTUAL: Unable to set the default  Channel Selection  value for 5Ghz using SNMP");
				}
				step++;// NEXT POST-CONDITION

			}

			LOGGER.info("POST-CONDITION " + step
					+ ": DESCRIPTION: Verify  setting  the default configuration of Max Concurrent Client Limit on 2.4 Ghz  using SNMP OID (.1.3.6.1.4.1.17270.50.2.2.2.1.1.11.10003)");

			LOGGER.info("POST-CONDITION " + step
					+ ": ACTION :  Execute the SNMP command to set the default Max Concurrent Client Limit on 2.4Ghz snmpset -v2c -c community_string udp6:<CMIP> .1.3.6.1.4.1.17270.50.2.2.2.1.1.11.10003 i 5");
			LOGGER.info("POST-CONDITION " + step
					+ ": EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.2.2.1.1.11.10003 = 5");

			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_MAX_CONCURRENT_CLIENTLIMIT_2_4_GHZ.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_FIVE,
					BroadBandSnmpMib.ECM_CONFIG_MAX_CONCURRENT_CLIENTLIMIT_2_4_GHZ.getTableIndex());
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_FIVE);
			if (status) {
				LOGGER.info("POST-CONDITION " + step
						+ " ACTUAL: The default  Max Concurrent Client Limit on 2.4 Ghz using SNMP was set successfully.");
			} else {
				LOGGER.error("POST-CONDITION " + step
						+ " ACTUAL: Unable to set the Max Concurrent Client Limit on 2.4 Ghz using SNMP");
			}
			step++;// NEXT POST-CONDITION
			LOGGER.info("POST-CONDITION " + step
					+ ": DESCRIPTION: Verify  setting  the default value  configuration of Max Concurrent Client Limit on  5Ghz  using SNMP OID (.1.3.6.1.4.1.17270.50.2.2.2.1.1.11.10103)");

			LOGGER.info("POST-CONDITION " + step
					+ ": ACTION :  Execute the SNMP command to set the default Max Concurrent Client Limit on 5Ghz snmpset -v2c -c community_string udp6:<CMIP> .1.3.6.1.4.1.17270.50.2.2.2.1.1.11.10103 i 5");
			LOGGER.info("POST-CONDITION " + step
					+ ": EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.2.2.1.1.11.10103 = 5");

			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_MAX_CONCURRENT_CLIENTLIMIT_5_GHZ.getOid(), SnmpDataType.INTEGER,
					BroadBandTestConstants.STRING_VALUE_FIVE,
					BroadBandSnmpMib.ECM_CONFIG_MAX_CONCURRENT_CLIENTLIMIT_5_GHZ.getTableIndex());
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_FIVE);
			if (status) {
				LOGGER.info("POST-CONDITION " + step
						+ " ACTUAL: The default  Max Concurrent Client Limit on 5Ghz using SNMP was set successfully.");
			} else {
				LOGGER.error("POST-CONDITION " + step
						+ " ACTUAL: Unable to set the default Max Concurrent Client Limit on 5Ghz using SNMP");
			}
			step++; // NEXT POST CONDITION
			LOGGER.info("POST-CONDITION " + step
					+ ": DESCRIPTION: Verify  setting  the default value for configuration of remote id and circuit id  using SNMP OID (1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3)");

			LOGGER.info("POST-CONDITION " + step
					+ ": ACTION :  Execute the SNMP SET command to set the default  remote id and circuit id snmpset -v2c -c community_string udp6:<CMIP> 1.3.6.1.4.1.17270.50.2.13.1.2.1.11.3 u 3");
			LOGGER.info("POST-CONDITION " + step
					+ ": EXPECTED : Expected output should be as, SNMPv2-SMI::enterprises.17270.50.2.13.1.2.1.11.3 = 3");

			response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getOid(), SnmpDataType.UNSIGNED_INTEGER,
					BroadBandTestConstants.STRING_VALUE_THREE,
					BroadBandSnmpMib.ECM_CONFIG_REMOTEID_CIRCUTID.getTableIndex());
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE);
			if (status) {
				LOGGER.info("POST-CONDITION " + step
						+ " ACTUAL: The default  remote id and Circuit id using SNMP was set successfully.");
			} else {
				LOGGER.error("POST-CONDITION " + step
						+ " ACTUAL: Unable to set the default remote id and Circuit id using SNMP");
			}

			LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");

			LOGGER.info("ENDING TEST CASE: TC-RDKB-WiFi-SNMP-1031");

		}
	}
}
